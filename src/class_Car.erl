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
-define(wooper_method_export, actSpontaneous/1, onFirstDiasca/2, update_path/3).

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
	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->	
  io:format("~n[INFO] Car acting spontaneous...~n"),
  Status = getAttribute(State, status),
  case Status of
    ready_to_travel ->
      io:format("~n[INFO] Ready to travel!~n"),
      next_hop(State);
    path_not_resolved ->
      CarName = getAttribute(State, car_name),
      io:format("~n[INFO] Path for car ~p not resolved!~n", [CarName]),
      resolve_path(State);
    finished ->
      CarName = getAttribute(State, car_name),
      io:format("~n[INFO] Travel for car ~p finished!~n", [CarName]),
      finish_trip(State);
    _ ->
      io:format("~n[ERROR] Don't know how to handle status ~p!~n", [Status]),
      executeOneway(State, declareTermination)
  end.

finish_trip(State) ->
  Type = getAttribute(State, type),
  Len = getAttribute(State, length),
  ST = getAttribute(State, start_time),
  Id = getAttribute(State, car_name),
  LastTick = class_Actor:get_current_tick_offset(State) + ST,
  [{graph_pid, G}] = ets:lookup(interscsimulator, graph_pid),
  V = getAttribute(State, current_node),
  io:format("~n[DEBUG] G: ~p~n", [G]),
  io:format("~n[DEBUG] V: ~p~n", [V]),
  {V, [LastNode]} = digraph:vertex(G, V),
  Mode = getAttribute(State, mode),
  print:write_final_message(Type, Len, ST, Id, LastTick, LastNode, Mode, csv),
  executeOneway(State, declareTermination).

resolve_path(State) ->
  Origin = getAttribute(State, origin),
  Destination = getAttribute(State, destination),
  [{city_graph_pid, GPid}] = ets:lookup(interscsimulator, city_graph_pid),
  S2 = class_Actor:send_actor_message(GPid, {calculate_bfs, {Origin, Destination}}, State),
  StartTime = getAttribute(State, start_time),
  T = class_Actor:get_current_tick_offset(S2) + StartTime,
  io:format("~n[DEBUG] Next action at tick ~p~n", [T]),
  executeOneway(S2, addSpontaneousTick, T).

next_hop(State) ->
  RemainingNodes = getAttribute(State, remaining_nodes),
  StartTime = getAttribute(State, start_time),
  io:format("~nStart time: ~p~n", [StartTime]),
  case RemainingNodes of
    [] ->
      S = setAttribute(State, status, finished),
      Time = class_Actor:get_current_tick_offset(S) + StartTime,
      io:format("~n[DEBUG] New next tick: ~p~n", [Time]),
      executeOneway(S, addSpontaneousTick, Time);
    [H|T] ->
      S1 = setAttribute(State, remaining_nodes, T),
      S2 = setAttribute(S1, current_node, H),
      io:format("~n[INFO] Actual vertex: ~p~n", [H]),
      Time = class_Actor:get_current_tick_offset(S2) + StartTime,
      io:format("~n[DEBUG] New next tick: ~p~n", [Time]),
      executeOneway(S2, addSpontaneousTick, Time)
  end.

	%% CurrentTickOffset = class_Actor:get_current_tick_offset( State ), 

	%% print:write_final_message( Type , TotalLength , StartTime , CarId , CurrentTickOffset , LastPosition , Mode , csv ),
	%% PathFinish = setAttribute( State , path , finish ),
  %%
	%% executeOneway( PathFinish , scheduleNextSpontaneousTick ).

%% request_position( State , _Trip , Path ) when Path == finish ->
%% 	CurrentTickOffset = class_Actor:get_current_tick_offset( State ),
%% 	Trips = getAttribute( State , trips ), 
%% 	NewTrips = list_utils:remove_element_at( Trips , 1 ),
%% 	
%% 	NewState = case length( NewTrips ) > 0 of
%% 		true -> 
%% 			InitialTrip = lists:nth( 1 , NewTrips ),	
%% 			NewPath = element( 2 , InitialTrip ),
%% 			setAttributes( State , [ { trips , NewTrips } , { path, NewPath} ] );
%% 		false -> 
%% 			setAttributes( State , [ { trips , NewTrips } , { path, ok} ] )
%% 	end,
%%
%% 	executeOneway( NewState , addSpontaneousTick , CurrentTickOffset + 1 );	


%% request_position( State , Trip , Path ) ->
%% 	case length( Path ) > 1 of
%% 		true ->	get_next_vertex( State , Path , element( 1 , Trip ) );
%% 		false -> verify_park( State , element( 1 , Trip ) )
%% 	end.
%%
%% verify_park( State , Mode ) when Mode == walk ->
%% 	FinalState = setAttribute( State, path , finish ),
%% 	executeOneway( FinalState , scheduleNextSpontaneousTick );
%%
%%
%% verify_park( State , _Mode ) ->						
%% 	DecrementVertex = getAttribute( State , last_vertex_pid ),	
%% 	ets:update_counter( list_streets , DecrementVertex , { 6 , -1 }),
%% 	ParkStatus = getAttribute( State , park_status ),
%%
%% 	case ParkStatus of
%%
%% 		not_parking ->
%% 			FinalState = setAttribute( State , path , finish ),
%%
%% 			executeOneway( FinalState , scheduleNextSpontaneousTick );
%% 		finish ->
%%
%% 			Park = getAttribute( State , park ),
%% 					
%% 			Parking = ets:lookup_element(options, parking_pid, 2 ),
%% 			NewState = class_Actor:send_actor_message( Parking, { spot_in_use, { Park } } , State ),
%% 									
%% 			FinalState = setAttribute( NewState, path , finish ),
%%
%% 			executeOneway( FinalState , scheduleNextSpontaneousTick );
%% 		find ->
%% 			Park = getAttribute( State , park ),
%% 			Parking = ets:lookup_element(options, parking_pid, 2 ),
%% 			class_Actor:send_actor_message( Parking, { spot_available, { Park } } , State )
%% 	end.
%%
%%
%% get_next_vertex( State , [ Current | Path ] , Mode ) when Mode == walk ->			
%% 	Vertices = list_to_atom( lists:concat( [ Current , lists:nth( 1 , Path ) ] )),
%% 	
%% 	Data = lists:nth( 1, ets:lookup( list_streets , Vertices ) ),
%% 	{ Id , Time , Distance } = traffic_models:get_speed_walk( Data ),
%% 	
%% 	TotalLength = getAttribute( State , distance ) + Distance,
%% 	FinalState = setAttributes( State , [ { distance , TotalLength } , { car_position , Id } , { path , Path } ] ), 
%%
%% %	print_movement( State ),
%%
%% 	executeOneway( FinalState , addSpontaneousTick , class_Actor:get_current_tick_offset( FinalState ) + Time );
%%
%% get_next_vertex( State , Path , _Mode ) ->
%%
%% 	Vertices = list_to_atom( lists:concat( [ lists:nth( 1 , Path )  , lists:nth( 2 , Path ) ] )),
%%
%% 	CurrentTick = class_Actor:get_current_tick_offset( State ),
%% 	
%% 	Data = lists:nth( 1, ets:lookup( list_streets , Vertices ) ),
%% 	{ _ , _ , _ , _ , _ , _ , From , _ , NumCars , Tick , MaxCar } = Data,
%%
%% 	case Tick /= CurrentTick of 
%% 		true ->
%%
%% 			ets:update_element( list_streets , Vertices , { 9 , 0 }),
%% 			ets:update_element( list_streets , Vertices , { 10 , CurrentTick });
%% 	
%% 		false ->
%%
%% 			ok
%%
%% 	end,
%%
%% 	case NumCars >= MaxCar of
%% 	
%% 		true ->
%%
%% 			FinalState = setAttribute( State , wait , true ),
%% 			executeOneway( FinalState , addSpontaneousTick , CurrentTick + 1 );
%%
%% 		false ->
%%
%% 			DecrementVertex = getAttribute( State , last_vertex_pid ),
%% 			case DecrementVertex of
%% 				ok ->
%% 					ok;
%% 				_ ->
%% 					ets:update_counter( list_streets, DecrementVertex , { 6 , -1 })
%% 			end,	
%%
%% 			ets:update_counter( list_streets , Vertices , { 6 , 1 }),
%%
%%             CurrentNode = lists:nth( 1 , Path ),
%%             CarName = getAttribute( State, car_name),
%%             EventEdge = ets:lookup( traffic_events, CurrentNode ),
%%
%%             NewPath = case EventEdge of
%%                 [ { CurrentNode, { FromNodeID, ToNodeID } } ] ->
%%                     case edgeInPath( Path, FromNodeID, ToNodeID ) of
%%                         true ->
%%                             io:format("PATH DO CARRO ~p MUDOU!~n", [CarName]),
%%                             [ { _ , CityGraph } ] = ets:lookup( graph , mygraph ),
%%                             [ { _, Origin } ] = ets:lookup(graph, atom_to_list(CurrentNode)),
%%                             [ ToNode | _ ] = lists:reverse( Path ),
%%                             [ { _, Destination } ] = ets:lookup(graph, atom_to_list(ToNode)),
%%                             NewVertices = digraph:get_short_path( CityGraph , Origin , Destination ),
%%                             Ids = getVerticesIds( CityGraph, NewVertices, [] ),
%%                             io:format("Path do ~p: ~p~n", [CarName, Ids]),
%%                             Ids;
%%                         false -> Path
%%                     end;
%%                 _ -> Path
%%             end,
%%
%% 			FinalPath = lists:nthtail( 1 , NewPath ),
%% 		
%% 			ets:update_counter( list_streets , Vertices , { 9 , 1 }),
%%
%% 			{ Id , Time , Distance } = traffic_models:get_speed_car( Data ),
%%
%% 			TotalLength = getAttribute( State , distance ) + Distance,
%% 			FinalState = setAttributes( State , [{ wait , false } , {distance , TotalLength} , {car_position , Id} , {last_vertex_pid , Vertices} , {path , FinalPath},  { coordFrom , From } ] ), 
%%
%% 			%	send data to rabbitMQ, including the From lat/long
%% 	
%%             { Lat, Lon } = From,
%%             LAT = lists:flatten( io_lib:format( "~p", [ Lat ] ) ),
%%             LON = lists:flatten( io_lib:format( "~p", [ Lon ] ) ),
%%             Uuid = getAttribute( State , uuid ),
%%             UUID = lists:flatten( io_lib:format( "~p", [ Uuid ] ) ),
%%             ID = lists:flatten( io_lib:format( "~p", [ atom_to_list(Id) ] ) ),
%%             TickSimulated = lists:flatten( io_lib:format( "~p", [ CurrentTick ] ) ),
%%
%%             { { Year, Month, Day }, { Hour, Minute, Second } } = calendar:local_time(),
%%             Timestamp = lists:flatten( io_lib:format( "~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",
%%                                                   [ Year, Month, Day, Hour, Minute, Second ] ) ),
%%
%%             RoutingKey = string:concat( Uuid, ".current_location.simulated" ),
%%
%%             Message = "{ \"uuid\": " ++ UUID ++
%%                       ", \"nodeID\": " ++ ID ++
%%                       ", \"lat\": " ++ LAT ++
%%                       ", \"lon\": " ++ LON ++
%%                       ", \"timestamp\": \"" ++ Timestamp ++
%%                       "\", \"tick\": " ++ TickSimulated ++ " }",
%%
%%             %print:publish_data( "data_stream", RoutingKey, Message ),
%%             %spawn( print, publish_data, [ "data_stream", RoutingKey, Message ] ),
%%             %SenderPid = whereis(message_sender_agent),
%%             [ { _, SenderPid } ] = ets:lookup( traffic_events, sender_pid ),
%%             %io:format("REGISTERED PROCESS: ~w~n", [registered()]),
%%             %io:format("SENDER PID: ~w~n", [SenderPid]),
%%             SenderPid ! { send_data, "data_stream", RoutingKey, Message },
%%
%% 			executeOneway( FinalState , addSpontaneousTick , CurrentTick + Time )
%%
%% 	end.
%%
%% getVerticesIds( _G, [], Ids ) ->
%%     Ids;
%% getVerticesIds( G, [ Vertex | Vertices ], Ids ) ->
%%     { _, { Id } } = digraph:vertex( G, Vertex),
%%     getVerticesIds( G, Vertices, lists:append( Ids, [ list_to_atom( Id ) ]) ).
%%
%% edgeInPath( [], _From, _To ) -> false;
%% edgeInPath( [ Node | Path ], From, To ) ->
%%     case Node =:= From of
%%         true ->
%% 		    case lists:nth( 1 , Path ) =:= To of
%%                 true -> true;
%% 				false -> false
%% 			end;
%% 		false -> edgeInPath( Path, From, To)
%% 	end.
%%
%%  
%% set_new_path( State , NewPath , _CityPID ) ->
%% 	Path = element( 1 , NewPath ), 
%% 	StateDict = setAttributes( State , [ { path , Path } , { park_status , finish } ] ),
%% 	Trips = getAttribute( StateDict , trips ), 
%% 	CurrentTrip = list_utils:get_element_at( Trips , 1 ),
%%         request_position( StateDict , CurrentTrip , Path ).

-spec onFirstDiasca(wooper:state(), pid()) -> oneway_return().
onFirstDiasca(State, _SendingActorPid) ->
	StartTime = getAttribute(State, start_time),
  FirstActionTime = class_Actor:get_current_tick_offset( State ) + StartTime,   	
	NewState = setAttribute(State, start_time , FirstActionTime),
	executeOneway(NewState, addSpontaneousTick, FirstActionTime).

update_path(State, Path, _Who) ->
  io:format("~n[INFO] New update_path! ~p~n", [Path]),
  S1 = setAttribute(State, remaining_nodes, Path),
  S2 = setAttribute(S1, status, ready_to_travel),
  ?wooper_return_state_only(S2).

%print_movement( State ) ->

%	LastPosition = getAttribute( State , car_position ),

%	{ Trips , CurrentTickOffset , CarId , Type , NewPosition }

 %            = { getAttribute( LengthState , trips ), class_Actor:get_current_tick_offset( State ) , 
  %               getAttribute( State , car_name ) , getAttribute( State , type ) , getAttribute( LengthState , car_position ) },
%	CurrentTrip =  lists:nth( 1 , Trips ),

%	FinalState = case LastPosition == -1 of

%		false ->
			
%			print:write_movement_car_message( LengthState , CarId , LastPosition , Type , ets:lookup_element(options, log_pid, 2 ) , CurrentTickOffset , NewPosition , csv  );
 

%		true -> 

%			LinkOrigin = element( 3 , CurrentTrip ), 

%			print:write_initial_message( LengthState , ets:lookup_element(options, log_pid, 2 ) , CarId , Type , CurrentTickOffset , LinkOrigin , LastPosition , csv )
	   
%	end.
