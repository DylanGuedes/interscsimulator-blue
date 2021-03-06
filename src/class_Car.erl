-module(class_Car).

-import('interscsimulator_utils', [print_error/2,
                                   print_success/2]).

-define(wooper_superclasses, [class_Actor]).
-define(wooper_construct_parameters, ActorSettings, CarMap).

-define(wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, destruct/1).

-define(wooper_method_export, actSpontaneous/1, onFirstDiasca/2,
        update_path/3, receive_append_result/3).

-include("smart_city_test_types.hrl").

-include("wooper.hrl").

-spec construct(wooper:state(), class_Actor:actor_settings(), map()) ->
  wooper:state().
construct(State, ?wooper_construct_parameters) ->
  {ok, Id} = maps:find(id, CarMap),
  {ok, V1} = maps:find(origin, CarMap),
  {ok, V2} = maps:find(destination, CarMap),
  {ok, Tick1} = maps:find(start_time, CarMap),
  {ok, Uuid} = maps:find(uuid, CarMap),

	ActorState = class_Actor:construct(State, ActorSettings, Id),

	setAttributes(ActorState, [
    { remaining_nodes_vtx, [] },
    { last_node_idx , -1 },
    { status, path_not_resolved },
		{ car_name, Id },
		{ distance , 0 },
    { length , 0},
		{ car_position, -1 },
		{ start_time , Tick1 },
		{ path, 2 },
    { origin_idx, V1 },
    { destination_idx, V2 },
    { current_edge_length, 0 },
    { uuid, Uuid }
  ]).

-spec destruct(wooper:state()) -> wooper:state().
destruct(State) ->
  State.

-spec actSpontaneous(wooper:state()) -> oneway_return().
actSpontaneous(State) ->
  Status = getAttribute(State, status),
  case Status of
    wait ->
      handle_wait(State);

    ready_to_travel ->
      next_hop(State);

    path_not_resolved ->
      resolve_path(State);

    finished ->
      executeOneway(State, declareTermination);

    _ ->
      print_error("Don't know how to handle status ~p!", [Status]),
      executeOneway(State, declareTermination)
  end.

% just wait til next tick
handle_wait(State) ->
  T = class_Actor:get_current_tick_offset(State),
	executeOneway(State, addSpontaneousTick, T+1).

% end trip and publish results
finish_trip(State) ->
  Len = getAttribute(State, distance),
  ST = getAttribute(State, start_time), % first tick
  Id = getAttribute(State, car_name),
  V1 = getAttribute(State, origin_idx),
  V2 = getAttribute(State, destination_idx),
  RWP = getAttribute(State, writer_pid),
  CurEdgeLen = getAttribute(State, current_edge_length),
  LastTick = class_Actor:get_current_tick_offset(State),
  R = {Id, Len+CurEdgeLen, ST, LastTick, V1, V2},
  UpdatedState = setAttribute(State, status, finished),
  S1 = class_Actor:send_actor_message(RWP, {append_to_output, R}, UpdatedState),
  ?wooper_return_state_only(S1).

% resolve bfs for car
resolve_path(State) ->
  V1Idx = getAttribute(State, origin_idx),
  V2Idx = getAttribute(State, destination_idx),
  GPid = whereis(singleton_city_graph),
  class_Actor:send_actor_message(GPid, {calculate_bfs, {V1Idx, V2Idx}}, State).

forbidden_edge(_V1_vtx, _V2_vtx) ->
  false.
  %% TODO> Commented this one out because it relies on ETS. ETS CAN'T BE
  %% DISTRIBUTED.
  %% [{graph_pid, G}] = ets:lookup(interscsimulator, graph_pid),
  %% {V1_vtx, V1Label} = digraph:vertex(G, V1_vtx),
  %% {V2_vtx, V2Label} = digraph:vertex(G, V2_vtx),
  %% [V1Idx] = V1Label,
  %% [V2Idx] = V2Label,
  %% [{{V1Idx, V2Idx}, E}] = ets:lookup(edges_pids, {V1Idx, V2Idx}),
  %% {E, V1_vtx, V2_vtx, L} = digraph:edge(G, E),
  %% {Id, _, _, _, _, _, _} = L,
  %% case ets:lookup(forbidden_edges, Id) of
  %%   [{forbidden_edges, Id}] ->
  %%     io:format("Edge with id ~p  is forbidden!!~n", [Id]),
  %%     true;
  %%   _ ->
  %%     false
  %% end.

% jump to next node
next_hop(State) ->
  RemainingNodes = getAttribute(State, remaining_nodes),

  case RemainingNodes of
    [] ->
      CurrentEdgeLength = getAttribute(State, current_edge_length),
      Distance = getAttribute(State, distance),
      LastState = setAttribute(State, distance, Distance + CurrentEdgeLength),
      finish_trip(LastState);

    [Current_vtx, Next_vtx | T] ->
      case forbidden_edge(Current_vtx, Next_vtx) of
        true ->
          io:format("[E] FORBIDDEN EDGE!!!~n");
        false ->
          handle_trip_walk(State, Current_vtx, Next_vtx, [Next_vtx|T])
      end;
    _ ->
      finish_trip(State)
  end.

handle_trip_walk(State, CurrentNode_vtx, NextNode_vtx, RemainingNodes) ->
  [{graph_pid, G}] = ets:lookup(interscsimulator, graph_pid),
  {CurrentNode_vtx, CurrentNodeLabel} = digraph:vertex(G, CurrentNode_vtx),
  {NextNode_vtx, NextNodeLabel} = digraph:vertex(G, NextNode_vtx),
  [CurrentNodeIdx] = CurrentNodeLabel,
  [NextNodeIdx] = NextNodeLabel,
  [{{CurrentNodeIdx, NextNodeIdx}, E_edg}] = ets:lookup(edges_pids, {CurrentNodeIdx, NextNodeIdx}),
  {E_edg, CurrentNode_vtx, NextNode_vtx, E_Label} = digraph:edge(G, E_edg),
  Capacity = element(4, E_Label),
  case Capacity of
    0 ->
      do_not_walk(State);
    _A when Capacity < 0 ->
      % can't walk to next node
      do_not_walk(State, Capacity);
    _ ->
      % can walk to next node. decrease edge (v1->v2), increase edge (last_node->v1)
      UpdatedState = setAttribute(State, remaining_nodes, RemainingNodes),
      LastNodeIdx = getAttribute(UpdatedState, last_node_idx),
      FinalState = case LastNodeIdx of
        -1 ->
          update_capacity(UpdatedState, CurrentNodeIdx, NextNodeIdx, -1);
        _ ->
          S1 = update_capacity(UpdatedState, CurrentNodeIdx, NextNodeIdx, -1),
          update_capacity(S1, LastNodeIdx, CurrentNodeIdx, 1)
      end,
      CurrentEdgeLength = getAttribute(FinalState, current_edge_length),
      Distance = getAttribute(FinalState, distance),
      LastState = setAttribute(FinalState, distance, Distance + CurrentEdgeLength),
      walk(setAttribute(LastState, last_node_idx, CurrentNodeIdx), E_Label)
  end.

% Updates capacity of edge label by factor. i.e: you can decrease or increase
update_capacity(State, V1Idx, V2Idx, Factor) when is_list(V1Idx), is_list(V2Idx), is_integer(Factor) ->
  GraphManagerPid = whereis(singleton_city_graph),
  class_Actor:send_actor_message(GraphManagerPid, {update_capacity, {V1Idx, V2Idx, Factor}}, State).

% Just wait til next tick
do_not_walk(State) ->
  T = class_Actor:get_current_tick_offset(State),
  executeOneway(State, addSpontaneousTick, T+1).
do_not_walk(State, Cap) ->
  T = class_Actor:get_current_tick_offset(State),
  executeOneway(State, addSpontaneousTick, T-Cap).


float_to_string(Float) ->
  io_lib:format("~p", [Float]).

walk(State, {EId, ELength, EFreeSpeed, ECapacity, _, _, _}=_Label) when is_float(ELength), is_float(EFreeSpeed) ->
  UpdatedState = setAttribute(State, current_edge_length, ELength),
  ElapsedTime = max(1, round(ELength / EFreeSpeed)),
  T = class_Actor:get_current_tick_offset(UpdatedState),
  WPid = whereis(result_writer_singleton),
	Payload = lists:flatten(io_lib:format("~s;~s;~s;~s;~p", [EId, float_to_string(ELength), float_to_string(EFreeSpeed), float_to_string(ECapacity), T])),
  UpdatedState2 = class_Actor:send_actor_message(WPid, {publish_event, {list_to_binary(Payload), <<"edge_update">>}}, UpdatedState),
  UpdatedState3 = send_to_data_processor(UpdatedState2),
  executeOneway(UpdatedState3, addSpontaneousTick, T+ElapsedTime).

send_to_data_processor(State) ->
  Uuid = getAttribute(State, uuid),
  NodeId = getAttribute(State, last_node_idx),
  T = class_Actor:get_current_tick_offset(State),
  WPid = whereis(result_writer_singleton),
  class_Actor:send_actor_message(WPid, {write_to_data_collector, {Uuid, NodeId, T}}, State).

check_replication(TablName, State) ->
  EtsGodPid = whereis(ets_holder),
  class_Actor:send_actor_message(EtsGodPid, {ensure_replica_is_healthy, TablName}, State).

-spec onFirstDiasca(wooper:state(), pid()) -> oneway_return().
onFirstDiasca(State, _SendingActorPid) ->
  InterscsimulatorEtsState = check_replication(interscsimulator, State),
  EdgesEtsState = check_replication(edges_pids, InterscsimulatorEtsState),
  NodesEtsState = check_replication(nodes_pids, EdgesEtsState),
	StartTime = getAttribute(NodesEtsState, start_time),
  CurrentTick = class_Actor:get_current_tick_offset(NodesEtsState),
  FirstActionTime = CurrentTick + StartTime,
	NewState = setAttribute(NodesEtsState, start_time, FirstActionTime),
  WPid = whereis(result_writer_singleton),
  S2 = setAttribute(NewState, writer_pid, WPid),
	executeOneway(add_path_to_solve(S2), addSpontaneousTick, FirstActionTime).

add_path_to_solve(State) ->
  V1Idx = getAttribute(State, origin_idx),
  V2Idx = getAttribute(State, destination_idx),
  GPid = whereis(singleton_city_graph),
  class_Actor:send_actor_message(GPid, {add_to_paths_to_solve, {V1Idx, V2Idx}}, State).

update_path(State, error, _Who) ->
  setAttribute(State, status, path_not_resolved);
update_path(State, Path, _Who) when is_list(Path) ->
  S1 = setAttribute(State, remaining_nodes, Path),
  S2 = setAttribute(S1, status, ready_to_travel),
  T = class_Actor:get_current_tick_offset(S2),
	executeOneway(S2, addSpontaneousTick, T+1).

receive_append_result(State, success, _WhoPid) ->
  TripId = getAttribute(State, car_name),
  print_success("Finishing trip for car ~p.", [TripId]),
  executeOneway(State, declareTermination).
