-module(class_CityGraph).

-define(wooper_superclasses, [class_Actor]).

-define(wooper_construct_parameters, ActorSettings, VerticesPath, EdgesPath).

-define(wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1).

-define(wooper_method_export, onFirstDiasca/2, calculate_bfs/3, is_ready/2).

-include("smart_city_test_types.hrl").

-include("wooper.hrl").

-spec construct(wooper:state(), class_Actor:actor_settings(),
                string(), string()) -> wooper:state().
construct(State, ?wooper_construct_parameters) ->
	ActorState = class_Actor:construct(State, ActorSettings, "City Graph"),
	setAttributes(ActorState, [{status, not_ready}]),
  ets:new(interscsimulator, [public, set, named_table]),
  ets:new(nodes_pids, [public, set, named_table]),
  G = digraph:new(),
  Nodes = extract_nodes_from_xml(VerticesPath),
  Links = extract_links_from_xml(EdgesPath),
  populate_graph_nodes(Nodes, G),
  populate_graph_links(Links, G),
  ets:insert(interscsimulator, {graph_pid, G}),
  ets:insert(interscsimulator, {city_graph_pid, self()}),
	setAttributes(ActorState, [{status, ready}]).

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
populate_graph_links([{U, V, L}|T], G) ->
  [{U, PidU}] = ets:lookup(nodes_pids, U),
  [{V, PidV}] = ets:lookup(nodes_pids, V), 
  digraph:add_edge(G, PidU, PidV, L),
  populate_graph_links(T, G).

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
  {From, To, {Id, Length, FreeSpeed, Capacity, PermLanes, Oneway, Modes}}.

-spec onFirstDiasca(wooper:state(), pid()) -> oneway_return().
onFirstDiasca(State, _SendingActorPid) ->
  [{graph_pid, G}] = ets:lookup(interscsimulator, graph_pid),
  io:format("[INFO] My graph: ~p~n", [G]),
  N = digraph:vertices(G),
  L = digraph:edges(G),
  io:format("[INFO] Nodes: ~p~n", [N]),
  io:format("[INFO] Links: ~p~n", [L]),
	?wooper_return_state_only(State).

calculate_bfs(State, {Origin, Destination}, WhoPid) ->
  io:format("~n[INFO] (From: CityGraph) -> Calculating Bfs~n"),
  [{Origin, U}] = ets:lookup(nodes_pids, Origin),
  [{Destination, V}] = ets:lookup(nodes_pids, Destination),
  [{graph_pid, G}] = ets:lookup(interscsimulator, graph_pid),
  Path = digraph:get_short_path(G, U, V),
  S1 = case Path of
    false ->
      io:format("~n[ERROR] No path is possible from ~p to ~p~n", [Origin, Destination]),
      class_Actor:send_actor_message(WhoPid, {update_path, error}, State);
    _ ->
      io:format("~n[INFO] Path found!~n"),
      class_Actor:send_actor_message(WhoPid, {update_path, [Path]}, State)
  end,
  io:format("[E] S1: ~n"),
  ?wooper_return_state_only(S1).

is_ready(State, WhoPid) ->
  case getAttribute(State, status) of
    ready ->
      class_Actor:send_actor_message(WhoPid, {ask_for_new_path}, State);
    _ ->
      class_Actor:send_actor_message(WhoPid, {wait_please}, State)
  end.
