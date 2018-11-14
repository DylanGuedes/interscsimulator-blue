-module(class_CityGraph).

-import('interscsimulator_utils', [print_error/2, print_info/2,
                                   print_success/2, print_info/1]).

-define(wooper_superclasses, [class_Actor]).
-define(wooper_construct_parameters, ActorSettings, VerticesPath, EdgesPath).
-define(wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1).
-define(wooper_method_export, onFirstDiasca/2, calculate_bfs/3, is_ready/2, update_capacity/3).

-include("smart_city_test_types.hrl").
-include("wooper.hrl").

create_ets_table(TableName, TableArgs) ->
  case ets:new(TableName, TableArgs) of
    TableName ->
      print_success("Sucessfully created ETS table ~p in actor ~p.", [TableName, "CityGraph"]);
    _ ->
      print_error("Couldn't create ETS table ~p in actor ~p.", [TableName, "CityGraph"])
  end.

-spec construct(wooper:state(), class_Actor:actor_settings(),
                string(), string()) -> wooper:state().
construct(State, ?wooper_construct_parameters) ->
	ActorState = class_Actor:construct(State, ActorSettings, "City Graph"),
	UpdatedState = setAttribute(ActorState, status, not_ready),

  case ets:info(interscsimulator) of
    undefined ->
      create_ets_table(interscsimulator, [public, set, named_table])
  end,

  create_ets_table(nodes_pids, [public, set, named_table]),
  create_ets_table(edges_pids, [public, set, named_table]),
  G = digraph:new(),
  Nodes = extract_nodes_from_xml(VerticesPath),
  Links = extract_links_from_xml(EdgesPath),
  populate_graph_nodes(Nodes, G),
  populate_graph_links(Links, G),
  ets:insert(interscsimulator, {graph_pid, G}),
  print_info("Inserting in table `interscsimulator` value `graph_pid`."),
  ets:insert(interscsimulator, {city_graph_pid, self()}),
  print_info("Inserting in table `interscsimulator` value `city_graph_pid`."),
	setAttribute(UpdatedState, status, ready).

-spec destruct(wooper:state()) -> wooper:state().
destruct(State) ->
	State.

populate_graph_nodes([], _G) ->
  ok;
populate_graph_nodes([{Id, _X, _Y}|T], G) ->
  V = digraph:add_vertex(G),
  digraph:add_vertex(G, V, [Id]),
  ets:insert(nodes_pids, {Id, V}),
  populate_graph_nodes(T, G).

populate_graph_links([], _G) ->
  ok;
populate_graph_links([{V1, V2, Label}|Tail], G) ->
  [{V1, PidV1}] = ets:lookup(nodes_pids, V1),
  [{V2, PidV2}] = ets:lookup(nodes_pids, V2), 
  E = digraph:add_edge(G, PidV1, PidV2, Label),
  ets:insert(edges_pids, {{V1, V2}, E}),
  populate_graph_links(Tail, G).

extract_nodes_from_xml(XmlPath) ->
  {Xml, _Misc} = xmerl_scan:file(XmlPath),
  SimplifiedXml = xmerl_lib:simplify_element(Xml),
  {nodes, _, InnerElement} = SimplifiedXml,
  do_parse_nodes(InnerElement, []).

do_parse_nodes([], Agg) ->
  Agg;
do_parse_nodes([H|T], Agg) ->
  case H of
    {node, Attrs, _} ->
      Node = mount_node(Attrs),
      do_parse_nodes(T, [Node | Agg]);
    _ ->
      do_parse_nodes(T, Agg)
  end.

mount_node(A) ->
  {id, Id} = lists:keyfind(id, 1, A),
  {x, X} = lists:keyfind(x, 1, A),
  {y, Y} = lists:keyfind(y, 1, A),
  {Id, X, Y}.

extract_links_from_xml(XmlPath) ->
  {Xml, _Misc} = xmerl_scan:file(XmlPath),
  SimplifiedXml = xmerl_lib:simplify_element(Xml),
  {links, _, InnerElement} = SimplifiedXml,
  do_parse_links(InnerElement, []).

do_parse_links([], Agg) ->
  Agg;
do_parse_links([H|T], Agg) ->
  case H of
    {link, Attrs, _} ->
      Link = mount_link(Attrs),
      do_parse_links(T, [Link | Agg]);
    _ ->
      do_parse_links(T, Agg)
  end.

mount_link(A) ->
  {id, Id} = lists:keyfind(id, 1, A),
  {from, From} = lists:keyfind(from, 1, A),
  {to, To} = lists:keyfind(to, 1, A),
  {length, Length} = lists:keyfind(length, 1, A),
  {freespeed, FreeSpeed} = lists:keyfind(freespeed, 1, A),
  {capacity, Capacity} = lists:keyfind(capacity, 1, A),
  {permlanes, PermLanes} = lists:keyfind(permlanes, 1, A),
  {oneway, Oneway} = lists:keyfind(oneway, 1, A),
  {modes, Modes} = lists:keyfind(modes, 1, A),
  {FloatFreeSpeed, _Rest} = string:to_float(FreeSpeed),
  {FloatLength, _Rest} = string:to_float(Length),
  {IntCapacity, _} = string:to_integer(Capacity),
  {From, To, {Id, FloatLength, FloatFreeSpeed, IntCapacity, PermLanes, Oneway, Modes}}.

-spec onFirstDiasca(wooper:state(), pid()) -> oneway_return().
onFirstDiasca(State, _SendingActorPid) ->
	?wooper_return_state_only(State).

calculate_bfs(State, {Origin, Destination}, WhoPid) ->
  [{Origin, U}] = ets:lookup(nodes_pids, Origin),
  [{Destination, V}] = ets:lookup(nodes_pids, Destination),
  [{graph_pid, G}] = ets:lookup(interscsimulator, graph_pid),
  Path = digraph:get_short_path(G, U, V),
  S1 = case Path of
    false ->
      print_error("No path is possible from ~p to ~p~n", [Origin, Destination]),
      class_Actor:send_actor_message(WhoPid, {update_path, error}, State);
    _ ->
      class_Actor:send_actor_message(WhoPid, {update_path, [Path]}, State)
  end,
  ?wooper_return_state_only(S1).

is_ready(State, WhoPid) ->
  case getAttribute(State, status) of
    ready ->
      class_Actor:send_actor_message(WhoPid, {ask_for_new_path}, State);
    _ ->
      class_Actor:send_actor_message(WhoPid, {wait_please}, State)
  end.

update_capacity(State, {V1Idx, V2Idx, Factor}, _WhoPid) ->
  [{{V1Idx, V2Idx}, E}] = ets:lookup(edges_pids, {V1Idx, V2Idx}),
  [{graph_pid, G}] = ets:lookup(interscsimulator, graph_pid),
  {E, V1_vtx, V2_vtx, Label} = digraph:edge(G, E),
  {X1, X2, X3, Capacity, X5, X6, X7} = Label,
  NewLabel = {X1, X2, X3, Capacity + Factor, X5, X6, X7},
  print_info("Updating capacity to ~p+(~p) for edge (~p)->(~p)", [Capacity, Factor, V1Idx, V2Idx]),
  digraph:add_edge(G, E, V1_vtx, V2_vtx, NewLabel),
	?wooper_return_state_only(State).
