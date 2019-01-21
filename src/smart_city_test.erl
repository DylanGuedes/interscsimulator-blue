-module(smart_city_test).

-include("test_constructs.hrl").

-record(singletons, {name, pid}).
-record(vertices, {vtx, pid}).
-record(edges, {v1, v2, pid}).

-spec run() -> no_return().
run() ->
	?test_start,

  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(singletons, [{attributes, record_info(fields, singleton_pids)}]),
  mnesia:create_table(vertices, [{attributes, record_info(fields, vertices)}]),
  mnesia:create_table(edges, [{attributes, record_info(fields, edges)}]),

	SimulationSettings = #simulation_settings{
    simulation_name = "Sim-Diasca Smart City Integration Test",
    initialisation_files = ["../assets/simulation_input/trips.init"],
    interactivity_mode = interactive,
    tick_duration = 1.0
  },

	DeploymentSettings = #deployment_settings{
    additional_elements_to_deploy = [ { ".", code } ],
    enable_performance_tracker = false
  },

	LoadBalancingSettings = #load_balancing_settings{},

  DeploymentManagerPid = sim_diasca:init(SimulationSettings, DeploymentSettings, LoadBalancingSettings),

	SimulationDuration = 100, % how many ticks? (in seconds)

	DeploymentManagerPid ! { getRootTimeManager, [], self() },
	RootTimeManagerPid = test_receive(),

	RootTimeManagerPid ! { startFor, [ SimulationDuration, self() ] },

	receive
    simulation_stopped ->
      ?test_info("Simulation stopped spontaneously, specified stop tick must have been reached.")
	end,

	?test_info("Browsing the report results, if in batch mode."),
	class_ResultManager:browse_reports(),

	?test_stop.
