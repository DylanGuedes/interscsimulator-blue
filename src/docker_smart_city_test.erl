-module(docker_smart_city_test).

-include("test_constructs.hrl").

-spec run() -> no_return().
run() ->
	?test_start,

	SimulationSettings = #simulation_settings{
    simulation_name = "(DOCKER) InterSCSimulator Test",
    initialisation_files = ["../assets/simulation_input/validation/docker_trips.init"],
    tick_duration = 1.0
  },

	DeploymentSettings = #deployment_settings{
    additional_elements_to_deploy = [ {".", code },
                                      {"../deps/brod/_build/default/lib/brod/ebin", data},
                                      {"../deps/brod/_build/default/lib/crc32cer/ebin", data},
                                      {"../deps/brod/_build/default/lib/kafka_protocol/ebin", data},
                                      {"../deps/brod/_build/default/lib/snappyer/ebin", data},
                                      {"../deps/brod/_build/default/lib/supervisor3/ebin", data},
                                      {"../assets/simulation_input/validation/nodes.xml", data},
                                      {"../assets/simulation_input/validation/links.xml", data}
                                      ],
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
