%Class that represents a person that can moves around the city graph on foot or by car
-module(class_CarManager).

% Determines what are the mother classes of this class (if any):
-define( wooper_superclasses, [ class_Actor ] ).

% parameters taken by the constructor ('construct').
-define( wooper_construct_parameters, ActorSettings, CarName , CarList, GraphManagerPid ).

% Declaring all variations of WOOPER-defined standard life-cycle operations:
% (template pasted, just two replacements performed to update arities)
-define( wooper_construct_export, new/4, new_link/4,
		 synchronous_new/4, synchronous_new_link/4,
		 synchronous_timed_new/4, synchronous_timed_new_link/4,
		 remote_new/5, remote_new_link/5, remote_synchronous_new/5,
		 remote_synchronous_new_link/5, remote_synchronisable_new_link/5,
		 remote_synchronous_timed_new/5, remote_synchronous_timed_new_link/5,
		 construct/5, destruct/1 ).

% Method declarations.
-define( wooper_method_export, actSpontaneous/1, onFirstDiasca/2 ).

% Allows to define WOOPER base variables and methods for that class:
-include("smart_city_test_types.hrl").

% Allows to define WOOPER base variables and methods for that class:
-include("wooper.hrl").

-include_lib("../deps/amqp_client/include/amqp_client.hrl").

% Creates a new agent that is a person that moves around the city
-spec construct( wooper:state(), class_Actor:actor_settings(),
				class_Actor:name() , parameter(), parameter() ) -> wooper:state().
construct( State, ?wooper_construct_parameters ) ->
	ActorState = class_Actor:construct( State, ActorSettings, CarName ),
	DictCars = create_dict( dict:new() , CarList ),

	Hostname = os:getenv( "RABBITMQ_HOST", "localhost" ),
	{ ok, Connection} = amqp_connection:start(#amqp_params_network{host=Hostname}),
	{ ok, Channel } = amqp_connection:open_channel( Connection ),

	setAttributes( ActorState, [ { car_list, DictCars }, { graph_manager, GraphManagerPid }, { connection, Connection}, { channel, Channel} ] ).

create_dict( Dict , [] ) -> Dict;
create_dict( Dict , [ Car | CarList ] ) ->
	{ Key , Value } = Car,
	Element = dict:find( Key , Dict ),
	
	NewDict = case Element of 
		error -> dict:store( Key , Value , Dict );
		{ ok , ListCar } -> dict:store( Key , ListCar ++ Value , Dict )
	end,
	create_dict( NewDict , CarList ).


-spec destruct( wooper:state() ) -> wooper:state().
destruct( State ) ->
	%Connection = getAttribute( State, connection ),
	Channel = getAttribute( State, channel ),
	amqp_channel:close(Channel),
	%amqp_connection:close(Connection),
	State.

-spec actSpontaneous( wooper:state() ) -> oneway_return().
actSpontaneous( State ) ->

	CurrentTick = class_Actor:get_current_tick_offset( State ),

	DictCars = getAttribute( State , car_list ),
	Cars = dict:find( CurrentTick , DictCars ),

	NewState = case Cars of
			   error -> State;
			   { ok , List } ->
				   Channel = getAttribute( State, channel ),
				   init_cars( List , State, Channel )
		   end,

	executeOneway( NewState , addSpontaneousTick , CurrentTick + 1 ).
	
init_cars( [] , State, _Channel ) -> State;
init_cars( [ Car | Cars ] , State, Channel ) ->

	{ CarName , ListTripsFinal , Type, Park , Mode , Count, Uuid } = Car,	

	CarFinalName = io_lib:format( "~s_~B", [ CarName , Count ] ),
	StartTime = class_RandomManager:get_uniform_value( 2000 ),

	GraphManagerPid = getAttribute( State, graph_manager ),
	NewState = class_Actor:create_actor( class_Car,
		[ CarFinalName , ListTripsFinal , StartTime , Type , Park , Mode, GraphManagerPid, Uuid, Channel ] , State ),

	%NewState = case Mode of
	%	car ->
	%		create_person_car( Count , State , CarName , ListTripsFinal , Type , Park , Mode, Uuid, Channel );
	%	walk ->	
	%		create_person_car( Count , State , CarName , ListTripsFinal , Type , Park , Mode, Uuid, Channel );
	%	_ ->
	%		create_person_public( Count , State , CarName , ListTripsFinal , Type , Mode )
	%end,
	init_cars( Cars , NewState, Channel ).


%create_person_car( 0 , State , _CarName , _ListTripsFinal , _Type , _Park , _Mode, _Uuid, _Channel ) -> State;
%create_person_car( Count , State , CarName , ListTripsFinal , Type , Park , Mode, Uuid, Channel ) ->
%	CarFinalName = io_lib:format( "~s_~B", [ CarName , Count ] ),
%	StartTime = class_RandomManager:get_uniform_value( 2000 ),
%
%	GraphManagerPid = getAttribute( State, graph_manager ),
%	NewState = class_Actor:create_actor( class_Car,
%		[ CarFinalName , ListTripsFinal , StartTime , Type , Park , Mode, GraphManagerPid, Uuid, Channel ] , State ),
%
%	create_person_car( Count - 1 , NewState , CarName , ListTripsFinal , Type , Park , Mode, Uuid, Channel ).
%
%
%create_person_public( _Count = 0 , State , _CarName , _ListTripsFinal , _Type , _Mode ) -> State;
%create_person_public( Count , State , CarName , ListTripsFinal , Type , Mode ) ->
%	CarFinalName = io_lib:format( "~s_~B", [ CarName , Count ] ),
%	StartTime = class_RandomManager:get_uniform_value( 1200 ),
%
%	NewState = class_Actor:create_actor( class_Person,
%		[ CarFinalName , ListTripsFinal , StartTime , Type , Mode ]  , State ),
%
%	create_person_public( Count - 1 , NewState , CarName , ListTripsFinal , Type , Mode ).

-spec onFirstDiasca( wooper:state(), pid() ) -> oneway_return().
onFirstDiasca( State, _SendingActorPid ) ->
    	FirstActionTime = class_Actor:get_current_tick_offset( State ) + 1,   	
	executeOneway( State , addSpontaneousTick , FirstActionTime ).
