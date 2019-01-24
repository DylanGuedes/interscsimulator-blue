-module(class_EtsGod).

-import('interscsimulator_utils', [print_error/2,
                                   print_success/2, print_info/2]).

-define(wooper_superclasses, [class_Actor]).
-define(wooper_construct_parameters, ActorSettings).
-define(wooper_construct_export, new/1, new_link/1,
		 synchronous_new/1, synchronous_new_link/1,
		 synchronous_timed_new/1, synchronous_timed_new_link/1,
		 remote_new/2, remote_new_link/2, remote_synchronous_new/2,
		 remote_synchronous_new_link/2, remote_synchronisable_new_link/2,
		 remote_synchronous_timed_new/2, remote_synchronous_timed_new_link/2,
		 construct/2, destruct/1).
-define(wooper_method_export, onFirstDiasca/2, ensure_replica_is_healthy/3, update_your_ets/3, update_your_digraph/3).

-include("smart_city_test_types.hrl").
-include("wooper.hrl").

create_ets_table(TableName, TableArgs) ->
  case ets:new(TableName, TableArgs) of
    TableName ->
      print_success("Sucessfully created ETS table ~p located in node ~p.", [TableName, node()]);
    _ ->
      print_error("Couldn't create ETS table ~p located in node ~p.", [TableName, node()])
  end.

-spec construct(wooper:state(), class_Actor:actor_settings()) -> wooper:state().
construct(State, ?wooper_construct_parameters) ->
  ActorState = class_Actor:construct(State, ActorSettings, "One EtsHolder"),
  TheHolder = case whereis(ets_holder) of
    undefined ->
      register(ets_holder, self()),
      true;
    _ ->
      false
  end,

  case TheHolder of
    true ->
      create_ets_table(interscsimulator, [public, set, named_table]),
      create_ets_table(nodes_pids, [public, set, named_table]),
      create_ets_table(edges_pids, [public, set, named_table]);
    false ->
      ok
  end,
	setAttributes(ActorState, [
    { status, ready },
    { interscsimulator , false },
    { edges_pids , false },
    { nodes_pids, false}
  ]).

-spec destruct(wooper:state()) -> wooper:state().
destruct(State) ->
	State.

-spec onFirstDiasca(wooper:state(), pid()) -> oneway_return().
onFirstDiasca(State, _SendingActorPid) ->
	?wooper_return_state_only(State).

ensure_replica_is_healthy(State, EtsTabl, _WhoPid) ->
  case empty_ets(EtsTabl) of
    true ->
      case getAttribute(State, EtsTabl) of
        true ->
          ?wooper_return_state_only(State);
        _ ->
          setAttribute(ask_for_ets_replication(EtsTabl, State), EtsTabl, true)
      end;
    false -> ?wooper_return_state_only(State)
  end.

ask_for_ets_replication(EtsTabl, State) ->
  MainProcess = global:whereis_name(singleton_city_graph),
  class_Actor:send_actor_message(MainProcess, {send_me_ets, EtsTabl}, State).

empty_ets(EtsTabl) ->
  case ets:first(EtsTabl)  of
    '$end_of_table' ->
      true;
    _ ->
      false
  end.

update_your_ets(State, {EtsTabl, EtsTablContent}, _WhoPid) ->
  case empty_ets(EtsTabl) of
    true ->
      [ets:insert(EtsTabl, {K, V}) || {K, V} <- EtsTablContent],
      case EtsTabl of
        interscsimulator ->
          print_info("Content: ~p", [EtsTablContent]),
          ask_for_digraph_replication(State);
        _ ->
          ?wooper_return_state_only(State)
      end;
    false ->
      ?wooper_return_state_only(State)
  end.

ask_for_digraph_replication(State) ->
  MainProcess = global:whereis_name(singleton_city_graph),
  class_Actor:send_actor_message(MainProcess, {send_me_digraph, now}, State).

deserialize_digraph({VL, EL, NL, B}) ->
  DG = case B of
         true -> digraph:new();
         false -> digraph:new([acyclic])
       end,
  {digraph, V, E, N, B} = DG,
  ets:delete_all_objects(V),
  ets:delete_all_objects(E),
  ets:delete_all_objects(N),
  ets:insert(V, VL),
  ets:insert(E, EL),
  ets:insert(N, NL),
  ets:insert(interscsimulator, {graph_pid, DG}),
  {digraph, V, E, N, B}.

update_your_digraph(State, {DigraphPayload}, _WhoPid) ->
  deserialize_digraph(binary_to_term(zlib:gunzip(DigraphPayload))),
  ?wooper_return_state_only(State).
