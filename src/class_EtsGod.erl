-module(class_EtsGod).

-import('interscsimulator_utils', [print_error/2, print_success/2]).

-define(wooper_superclasses, [class_Actor]).
-define(wooper_construct_parameters, ActorSettings).
-define(wooper_construct_export, new/1, new_link/1,
		 synchronous_new/1, synchronous_new_link/1,
		 synchronous_timed_new/1, synchronous_timed_new_link/1,
		 remote_new/2, remote_new_link/2, remote_synchronous_new/2,
		 remote_synchronous_new_link/2, remote_synchronisable_new_link/2,
		 remote_synchronous_timed_new/2, remote_synchronous_timed_new_link/2,
		 construct/2, destruct/1).
-define(wooper_method_export, onFirstDiasca/2).

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
  setAttribute(ActorState, status, ready).

-spec destruct(wooper:state()) -> wooper:state().
destruct(State) ->
	State.

-spec onFirstDiasca(wooper:state(), pid()) -> oneway_return().
onFirstDiasca(State, _SendingActorPid) ->
	?wooper_return_state_only(State).
