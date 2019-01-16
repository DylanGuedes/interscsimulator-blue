-module(class_GraphTrigger).

-define(wooper_superclasses, [class_Actor]).

-define(wooper_construct_parameters, ActorSettings, TriggerInfo).

-define(wooper_construct_export, new/2, new_link/2,
		 synchronous_new/2, synchronous_new_link/2,
		 synchronous_timed_new/2, synchronous_timed_new_link/2,
		 remote_new/3, remote_new_link/3, remote_synchronous_new/3,
		 remote_synchronous_new_link/3, remote_synchronisable_new_link/3,
		 remote_synchronous_timed_new/3, remote_synchronous_timed_new_link/3,
		 construct/3, destruct/1).

-define(wooper_method_export, onFirstDiasca/2, actSpontaneous/1).

-include("smart_city_test_types.hrl").

-include("wooper.hrl").

-spec construct(wooper:state(), class_Actor:actor_settings(),
                string()) -> wooper:state().
construct(State, ?wooper_construct_parameters) ->
  {ok, TriggerType} = maps:find(triggertype, TriggerInfo),
  {ok, TriggerId} = maps:find(id, TriggerInfo),
  {ok, EdgeId} = maps:find(edgeid, TriggerInfo),
  {ok, StartTick} = maps:find(starttick, TriggerInfo),
  {ok, EndTick} = maps:find(endtick, TriggerInfo),
  true = (StartTick > 2),
  true = (EndTick > StartTick),
	ActorState = class_Actor:construct(State, ActorSettings, "Trigger " ++ TriggerId),
	setAttributes(ActorState, [
    {type, TriggerType},
    {id, TriggerId},
    {edgeid, EdgeId},
    {starttick, StartTick},
    {endtick, EndTick},
    {status, waiting_to_start}
  ]).

-spec destruct(wooper:state()) -> wooper:state().
destruct(State) ->
  State.

-spec onFirstDiasca(wooper:state(), pid()) -> oneway_return().
onFirstDiasca(State, _SendingActorPid) ->
  TriggerStartTick = getAttribute(State, starttick),
	executeOneway(State, addSpontaneousTick, TriggerStartTick).

-spec actSpontaneous(wooper:state()) -> oneway_return().
actSpontaneous(State) ->
  Status = getAttribute(State, status),
  handle_status(State, Status),
  ?wooper_return_state_only(State).

handle_status(State, waiting_to_start) ->
  T = class_Actor:get_current_tick_offset(State),
  StartTick = getAttribute(State, starttick),
  T = StartTick,
  TriggerType = getAttribute(State, triggertype),
  UpdatedState = setAttribute(State, status, triggered),
  TriggeredState =
    case TriggerType of
      block_edge ->
        trigger_edge_block(UpdatedState);
      _ ->
        UpdatedState
    end,
  TriggerEndTick = getAttribute(State, endtick),
  executeOneway(TriggeredState, addSpontaneousTick, TriggerEndTick).

trigger_edge_block(State) ->
  [{city_graph_pid, GPid}] = ets:lookup(interscsimulator, city_graph_pid),
  EdgeId = getAttribute(State, edgeid),
  Until = getAttribute(State, endtick),
  class_Actor:send_actor_message(GPid, {block_edge, EdgeId, Until}, State).
