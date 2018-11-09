%Class that represents a person that can moves around the city graph on foot or by car
-module(class_Car).

-define(wooper_superclasses, [class_Actor]).
-define(wooper_construct_parameters, ActorSettings, CarMap).

-define(wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, destruct/1).

-define(wooper_method_export, actSpontaneous/1, onFirstDiasca/2, update_path/3, receive_append_result/3).

-include("smart_city_test_types.hrl").

-include("wooper.hrl").

-spec construct(wooper:state(), class_Actor:actor_settings(), map()) ->
  wooper:state().
construct(State, ?wooper_construct_parameters) ->
  {ok, Id} = maps:find(id, CarMap),
  {ok, V1} = maps:find(origin, CarMap),
  {ok, V2} = maps:find(destination, CarMap),
  {ok, Tick1} = maps:find(start_time, CarMap),

	ActorState = class_Actor:construct(State, ActorSettings, Id),

	setAttributes(ActorState, [
    { remaining_nodes, [] },
    { current_node, -1 },
    { status, path_not_resolved },
		{ car_name, Id },
		{ distance , 0 },
    { length , 0},
		{ car_position, -1 },
		{ start_time , Tick1 },
		{ path , 2 },
		{ last_vertex_pid , ok },
		{ coordFrom , ok },
		{ wait , false },
    { origin, V1 },
    { destination, V2 }
  ]).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
  State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->	
  _CurrentTick = class_Actor:get_current_tick_offset(State),
  Status = getAttribute(State, status),
  case Status of
    ready_to_travel ->
      next_hop(State);
    path_not_resolved ->
      resolve_path(State);
    _ ->
      io:format("~n[ERROR] Don't know how to handle status ~p!~n", [Status]),
      executeOneway(State, declareTermination)
  end.

finish_trip(State) ->
  Len = getAttribute(State, length),
  ST = getAttribute(State, start_time),
  Id = getAttribute(State, car_name),
  V1 = getAttribute(State, origin),
  V2 = getAttribute(State, destination),
  LastTick = class_Actor:get_current_tick_offset(State),
  R = {Len, ST, Id, LastTick, V1, V2},
  RWP = getAttribute(State, writer_pid),
  S1 = class_Actor:send_actor_message(RWP, {append_to_output, R}, State),
  setAttribute(S1, status, finished).

resolve_path(State) ->
  V1 = getAttribute(State, origin),
  V2 = getAttribute(State, destination),
  [{city_graph_pid, GPid}] = ets:lookup(interscsimulator, city_graph_pid),
  S2 = class_Actor:send_actor_message(GPid, {calculate_bfs, {V1, V2}}, State),
	?wooper_return_state_only(S2).

next_hop(State) ->
  RemainingNodes = getAttribute(State, remaining_nodes),
  case RemainingNodes of
    [] ->
      finish_trip(State);

    [Current, Next | T] ->
      S1 = setAttribute(State, remaining_nodes, [Next|T]),
      S2 = setAttribute(S1, current_node, Current),
      [{graph_pid, G}] = ets:lookup(interscsimulator, graph_pid),
      {Current, [V1]} = digraph:vertex(G, Current),
      {Next, [V2]} = digraph:vertex(G, Next),
      [{{V1, V2}, EdgePid}] = ets:lookup(edges_pids, {V1, V2}),
      {_, _, _, EdgeLabel} = digraph:edge(G, EdgePid),
      {_, ELength, EFreeSpeed, _, _, _, _} = EdgeLabel,
      ElapsedTime = max(1, round(ELength / EFreeSpeed)),
      Time = class_Actor:get_current_tick_offset(S2) + ElapsedTime,
      executeOneway(S2, addSpontaneousTick, Time);

    [H|T] ->
      S1 = setAttribute(State, remaining_nodes, T),
      S2 = setAttribute(S1, current_node, H),
      Time = class_Actor:get_current_tick_offset(S2) + 1,
      executeOneway(S2, addSpontaneousTick, Time)
  end.

-spec onFirstDiasca(wooper:state(), pid()) -> oneway_return().
onFirstDiasca(State, _SendingActorPid) ->
	StartTime = getAttribute(State, start_time),
  CurrentTick = class_Actor:get_current_tick_offset(State),
  FirstActionTime = CurrentTick + StartTime,
	NewState = setAttribute(State, start_time, FirstActionTime),
  [{result_writer_pid, WPid}] = ets:lookup(interscsimulator, result_writer_pid),
  S2 = setAttribute(NewState, writer_pid, WPid),
	executeOneway(S2, addSpontaneousTick, FirstActionTime).

update_path(State, Path, _Who) ->
  S1 = setAttribute(State, remaining_nodes, Path),
  S2 = setAttribute(S1, status, ready_to_travel),
  T = class_Actor:get_current_tick_offset(S2),
	executeOneway(S2, addSpontaneousTick, T+1).

receive_append_result(State, success, _WhoPid) ->
  executeOneway(State, declareTermination).
