% Author: Eduardo Santana (efzambom@ime.usp.br)

-module(smart_city_test).

% For all facilities common to all tests:
-include("test_constructs.hrl").	

readConfigPath() ->
	{ok, Device} = file:open('../interscsimulator.conf', [read]),
	{ok, Data} = file:read_line(Device),
	string:chomp(Data).

-spec run() -> no_return().
run() ->	
	?test_start,
	
	% Use default simulation settings (50Hz, batch reproducible):
	SimulationSettings = #simulation_settings{
    simulation_name = "Sim-Diasca Smart City Integration Test",
    tick_duration = 1,
    initialisation_files = ["trips.init"]
  },

	DeploymentSettings = #deployment_settings{
    additional_elements_to_deploy = [ { ".", code } ],
    enable_performance_tracker = false
  },

	% Default load balancing settings (round-robin placement heuristic):
	LoadBalancingSettings = #load_balancing_settings{},

	% A deployment manager is created directly on the user node:
  DeploymentManagerPid = sim_diasca:init( SimulationSettings, DeploymentSettings, LoadBalancingSettings ),

	ConfigPath = readConfigPath(),

	Config = config_parser:load_config_from_xml( ConfigPath ),

	SimulationDuration = element( 1 , string:to_integer(element( 2 , Config ) ) ),

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	?test_info_fmt( "Starting simulation, for a stop after a duration "
					"in virtual time of ~Bms.", [ SimulationDuration ] ),

	RootTimeManagerPid ! { startFor, [ SimulationDuration, self() ] },

	?test_info( "Waiting for the simulation to end, "
				"since having been declared as a simulation listener." ),

	receive

		simulation_stopped ->

			?test_info( "Simulation stopped spontaneously, "
						"specified stop tick must have been reached." )

	end,

	?test_info( "Browsing the report results, if in batch mode." ),
	class_ResultManager:browse_reports(),

	?test_stop.
