-module(class_TickKeeper).

%-import('interscsimulator_utils', [print_error/2, print_success/2]).

-define(wooper_superclasses, [class_Actor]).

-define(wooper_construct_parameters, ActorSettings).

-define(wooper_construct_export, new/1, new_link/1,
        synchronous_new/1, synchronous_new_link/1,
        synchronous_timed_new/1, synchronous_timed_new_link/1,
        remote_new/2, remote_new_link/2, remote_synchronous_new/2,
        remote_synchronous_new_link/2, remote_synchronisable_new_link/2,
        remote_synchronous_timed_new/2, remote_synchronous_timed_new_link/2,
        construct/2, destruct/1).

-define(wooper_method_export, actSpontaneous/1, onFirstDiasca/2).

-include("wooper.hrl").

construct(State, ?wooper_construct_parameters) ->
  class_Actor:construct(State, ActorSettings, "Keeper").

onFirstDiasca(State, _SendingActorPid) ->
  ?wooper_return_state_only(State).

actSpontaneous(State) ->
  executeOneway(State, declareTermination).

destruct(State) ->
  State.
