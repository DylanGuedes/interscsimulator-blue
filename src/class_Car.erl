%Class that represents a person that can moves around the city graph on foot or by car
-module(class_Car).

-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define(wooper_construct_parameters, ActorSettings, CarName , Origin, Destination, _LinkOrigin, StartTime, Type, _Park, Mode, Uuid).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/10, new_link/10,
		 synchronous_new/10, synchronous_new_link/10,
		 synchronous_timed_new/10, synchronous_timed_new_link/10,
		 remote_new/11, remote_new_link/11, remote_synchronous_new/11,
		 remote_synchronous_new_link/11, remote_synchronisable_new_link/11,
		 remote_synchronous_timed_new/11, remote_synchronous_timed_new_link/11,
		 construct/11, destruct/1 ).

% Method declarations.
-define(wooper_method_export, actSpontaneous/1, onFirstDiasca/2, update_path/3, receive_append_result/3).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name(), pid() , parameter() , parameter(), parameter() , parameter() , parameter(), parameter(), parameter() ) -> wooper:state().
construct(State, ?wooper_construct_parameters) ->
	ActorState = class_Actor:construct(State, ActorSettings, CarName),

	setAttributes(ActorState, [
    { remaining_nodes, [] },
    { current_node, -1 },
    { status, path_not_resolved },
		{ car_name, CarName },
		{ type, Type },
		{ distance , 0 },
    { length , 0},
		{ car_position, -1 },
		{ start_time , StartTime },
		{ path , 2 },
		{ mode , Mode },
		{ last_vertex_pid , ok },
		{ coordFrom , ok },
		{ wait , false },
		{ uuid, Uuid },
    { origin, Origin },
    { destination, Destination }
  ]).

-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
  io:format("~n[INFO] Destructing car...~n"),
  State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->	
  CurrentTick = class_Actor:get_current_tick_offset(State),
  io:format("~n[INFO] Car acting spontaneous at tick ~p...~n", [CurrentTick]),
  Status = getAttribute(State, status),
  case Status of
    ready_to_travel ->
      next_hop(State);
    path_not_resolved ->
      CarName = getAttribute(State, car_name),
      io:format("~n[INFO] Path for car ~p not resolved!~n", [CarName]),
      resolve_path(State);
    _ ->
      io:format("~n[ERROR] Don't know how to handle status ~p!~n", [Status]),
      executeOneway(State, declareTermination)
  end.

finish_trip(State) ->
  Type = getAttribute(State, type),
  Len = getAttribute(State, length),
  ST = getAttribute(State, start_time),
  Id = getAttribute(State, car_name),
  LastTick = class_Actor:get_current_tick_offset(State),
  R = {Type, Len, ST, Id, LastTick},
  RWP = getAttribute(State, writer_pid),
  S1 = class_Actor:send_actor_message(RWP, {append_to_output, R}, State),
  io:format("~n[INFO] Sending `append_to_output` call to result writer!~n"),
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
      io:format("~n[DEBUG] New next tick: ~p~n", [Time]),
      executeOneway(S2, addSpontaneousTick, Time);

    [H|T] ->
      S1 = setAttribute(State, remaining_nodes, T),
      S2 = setAttribute(S1, current_node, H),
      Time = class_Actor:get_current_tick_offset(S2) + 1,
      io:format("~n[DEBUG] New next tick: ~p~n", [Time]),
      executeOneway(S2, addSpontaneousTick, Time)
  end.

-spec onFirstDiasca(wooper:state(), pid()) -> oneway_return().
onFirstDiasca(State, _SendingActorPid) ->
	StartTime = getAttribute(State, start_time),
  CurrentTick = class_Actor:get_current_tick_offset(State),
  FirstActionTime = CurrentTick + StartTime,   	
	NewState = setAttribute(State, start_time , FirstActionTime),
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
