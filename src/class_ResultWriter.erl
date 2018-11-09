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

-define(wooper_method_export, onFirstDiasca/2, append_to_output/3, actSpontaneous/1).

-include("smart_city_test_types.hrl").

-include("wooper.hrl").

-spec construct(wooper:state(), class_Actor:actor_settings(),
                string()) -> wooper:state().
construct(State, ?wooper_construct_parameters) ->
  ets:insert(interscsimulator, {result_writer_pid, self()}),
	ActorState = class_Actor:construct(State, ActorSettings, "Trips Result Writer"),
	setAttributes(ActorState, [
    {status, not_ready}, {final_path, FinalPath}, {file_ptr, -1}]).

-spec destruct(wooper:state()) -> wooper:state().
destruct(State) ->
  io:format("~nTERMINANDO O RESULT WRITER!~n"),
  FilePtr = getAttribute(State, file_ptr),
	file_utils:close(FilePtr),
	State.

-spec onFirstDiasca(wooper:state(), pid()) -> oneway_return().
onFirstDiasca(State, _SendingActorPid) ->
  FilePath = getAttribute(State, final_path),
	filelib:ensure_dir(FilePath),
	FilePtr = file_utils:open(FilePath, _Opts=[write, delayed_write]),
  S1 = setAttribute(State, file_ptr, FilePtr),
  io:format("~nEXECUTE ONE WAY RESULT WRITER~n"),
  T = class_Actor:get_current_tick_offset(State),
	executeOneway(S1, addSpontaneousTick, T+1000).

append_to_output(State, Payload, WhoPid) ->
  io:format("~n[INFO] Appending to output!~n"),
  {Type, Len, ST, Id, LastTick} = Payload,
	B = io_lib:format("~w;~w;~w;~w;~w\n", [Type, Len, ST, Id, LastTick]),
  FilePtr = getAttribute(State, file_ptr),
	file_utils:write(FilePtr, B),
  class_Actor:send_actor_message(WhoPid, {receive_append_result, success}, State).

-spec actSpontaneous(wooper:state()) -> oneway_return().
actSpontaneous(State) ->
  CurrentTick = class_Actor:get_current_tick_offset(State),
  io:format("~n[INFO] Current Tick at result writer: ~p~n", [CurrentTick]),
  ?wooper_return_state_only(State).
