-module(class_ResultWriter).

-define(wooper_superclasses, [class_Actor]).

-define(wooper_construct_parameters, ActorSettings, FinalPath).

-define(wooper_construct_export, new/2, new_link/2,
        synchronous_new/2, synchronous_new_link/2,
        synchronous_timed_new/2, synchronous_timed_new_link/2,
        remote_new/3, remote_new_link/3, remote_synchronous_new/3,
        remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
        remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
        construct/3, destruct/1).

-define(wooper_method_export, onFirstDiasca/2, append_to_output/3, actSpontaneous/1, publish_event/3, write_to_data_collector/3).

-include("smart_city_test_types.hrl").

-include_lib("../deps/amqp_client/include/amqp_client.hrl").

-include("wooper.hrl").

setup_kafka() ->
  case os:getenv("KAFKA_BUILD_PATH") of
    false ->
      throw("Please, set environment variable `KAFKA_BUILD_PATH` with the _build path of your brod package!");
    _ ->
      ok
  end,
  case os:getenv("KAFKA_HOST") of
    false ->
      throw("Please, set environment variable `KAFKA_HOST` with the ip of your kafka host!");
    _ ->
      ok
  end,
  case os:getenv("KAFKA_PORT") of
    false ->
      throw("Please, set environment variable `KAFKA_PORT` with the number of your kafka port!");
    _ ->
      ok
  end,
  {KafkaPort, _} = string:to_integer(os:getenv("KAFKA_PORT")),
  BuildPath = os:getenv("KAFKA_BUILD_PATH"),
  KafkaProtocolPath = BuildPath ++ "/default/lib/kafka_protocol/ebin",
  SnappyerPath = BuildPath ++ "/default/lib/snappyer/ebin",
  CrcPath = BuildPath ++ "/default/lib/crc32cer/ebin",
  Supervisor3Path = BuildPath ++ "/default/lib/supervisor3/ebin",
  BrodPath = BuildPath ++ "/default/lib/brod/ebin",
  Paths = [BrodPath, KafkaProtocolPath, SnappyerPath, CrcPath, Supervisor3Path],
  code:add_pathsa(Paths),
  {ok, _} = application:ensure_all_started(brod),
  KafkaBootstrapEndpoints = [{os:getenv("KAFKA_HOST"), KafkaPort}],
  ok = brod:start_client(KafkaBootstrapEndpoints, interscity_connection),
  brod:start_producer(interscity_connection, <<"simulation-events">>, []).

-spec construct(wooper:state(), class_Actor:actor_settings(),
                string()) -> wooper:state().
construct(State, ?wooper_construct_parameters) ->
  setup_kafka(),
  register(result_writer_singleton, self()),
  ActorState = class_Actor:construct(State, ActorSettings, "Trips Result Writer"),
  {ok, RabbitmqChannel} = setup_rabbitmq(),
  setAttributes(ActorState, [{status, not_ready}, {final_path, FinalPath}, {file_ptr, -1}, {rabbitmq_channel, RabbitmqChannel}]).

setup_rabbitmq() ->
	AmqpClientPath = os:getenv("AMQP_CLIENT_PATH"),
	Paths = [
    AmqpClientPath,
    AmqpClientPath ++ "/ebin",
    AmqpClientPath ++ "/include/rabbit_common/ebin"],
  code:add_pathsa(Paths),
  Hostname = os:getenv("RABBITMQ_HOST"),
	{ok, Connection} = amqp_connection:start(#amqp_params_network{host=Hostname}),
	amqp_connection:open_channel(Connection).

-spec destruct(wooper:state()) -> wooper:state().
destruct(State) ->
  close_results_file(State).

-spec close_results_file(wooper:state()) -> wooper:state().
close_results_file(State) ->
  FilePtr = getAttribute(State, file_ptr),
  file_utils:close(FilePtr),
  State.

-spec onFirstDiasca(wooper:state(), pid()) -> oneway_return().
onFirstDiasca(State, _SendingActorPid) ->
  FilePath = getAttribute(State, final_path),
  filelib:ensure_dir(FilePath),
  FilePtr = file_utils:open(FilePath, _Opts=[write, delayed_write]),
  S1 = setAttribute(State, file_ptr, FilePtr),
  T = class_Actor:get_current_tick_offset(State),
  B = io_lib:format("id;length;start_time;last_tick;origin;destination\n", []),
  file_utils:write(FilePtr, B),
  executeOneway(S1, addSpontaneousTick, T+1000).

append_to_output(State, Payload, WhoPid) ->
  {Id, Len, ST, LastTick, V1, V2} = Payload,
  B = io_lib:format("~s;~w;~w;~w;~s;~s\n", [Id, Len, ST, LastTick, V1, V2]),
  FilePtr = getAttribute(State, file_ptr),
  file_utils:write(FilePtr, B),
  class_Actor:send_actor_message(WhoPid, {receive_append_result, success}, State).

publish_event(State, {Payload, EventType}, _WhoPid) ->
  write_to_kafka(EventType, Payload, State).

write_to_data_collector(State, Payload, _WhoPid) ->
  write_to_rabbitmq(Payload, State).

write_to_kafka(EventType, Payload, State) ->
  Client = interscity_connection,
  Topic  = <<"simulation-events">>,
  brod:produce_sync(Client, Topic, 0, EventType, Payload),
  ?wooper_return_state_only(State).

write_to_rabbitmq({Uuid, NodeId, Tick}, State) ->
  Message = list_to_binary(lists:flatten( io_lib:format( "{ \"uuid\": ~p, \"nodeID\": ~p, \"tick\": ~p, \"date\": ~p}", [ Uuid, NodeId, Tick, os:cmd("date") ] ) )),
  RoutingKey = list_to_binary(Uuid ++ ".city_traffic.simulated"),
  Channel = getAttribute(State, rabbitmq_channel),
  Exchange = #'exchange.declare'{exchange = <<"data_stream">>, type = <<"topic">>},
  #'exchange.declare_ok'{} = amqp_channel:call(Channel, Exchange),
  Publish = #'basic.publish'{exchange = <<"data_stream">>, routing_key = RoutingKey},
  amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Message}),
  ?wooper_return_state_only(State).

-spec actSpontaneous(wooper:state()) -> oneway_return().
actSpontaneous(State) ->
  ?wooper_return_state_only(State).
