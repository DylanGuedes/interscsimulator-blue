-module(class_CityGraph).

-import('interscsimulator_utils', [print_error/2, print_info/2, validate_file/1]).

-define(wooper_superclasses, [class_Actor]).
-define(wooper_construct_parameters, ActorSettings, VerticesPath, EdgesPath).
-define(wooper_construct_export, new/3, new_link/3,
		 synchronous_new/3, synchronous_new_link/3,
		 synchronous_timed_new/3, synchronous_timed_new_link/3,
		 remote_new/4, remote_new_link/4, remote_synchronous_new/4,
		 remote_synchronous_new_link/4, remote_synchronisable_new_link/4,
		 remote_synchronous_timed_new/4, remote_synchronous_timed_new_link/4,
		 construct/4, destruct/1, send_me_ets/3, send_me_digraph/3).
-define(wooper_method_export, onFirstDiasca/2, calculate_bfs/3, update_capacity/3,
       add_to_paths_to_solve/3, actSpontaneous/1).

-include("smart_city_test_types.hrl").
-include("wooper.hrl").

-spec construct(wooper:state(), class_Actor:actor_settings(),
                string(), string()) -> wooper:state().
construct(State, ?wooper_construct_parameters) ->
  validate_file(VerticesPath),
  validate_file(EdgesPath),
  ActorState = class_Actor:construct(State, ActorSettings, "City Graph"),
  case whereis(singleton_city_graph) of
    undefined ->
      register(singleton_city_graph, self()),
      G = digraph:new(),
      Nodes = extract_nodes_from_xml(VerticesPath),
      Links = extract_links_from_xml(EdgesPath),
      populate_graph_nodes(Nodes, G),
      populate_graph_links(Links, G),
      setAttributes(ActorState, [
        { status, not_ready },
        { erl_graph_pid , G },
        { stay_alive, true }
      ]);
    _ ->
      setAttribute(ActorState, stay_alive, false)
  end.

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
  case getAttribute(State, stay_alive) of
    true ->
      G = getAttribute(State, erl_graph_pid),
      ets:insert(interscsimulator, {graph_pid, G}),
      print_info("Inserting `graph_pid` on table `interscsimulator` on node ~p.", [node()]),
      S1 = setAttribute(State, status, ready),
      T = class_Actor:get_current_tick_offset(State),
      executeOneway(S1, addSpontaneousTick, T+1);
    false ->
      executeOneway(State, declareTermination)
  end.

calculate_bfs(State, {Origin, Destination}, WhoPid) ->
  [{Origin, U}] = ets:lookup(nodes_pids, Origin),
  [{Destination, V}] = ets:lookup(nodes_pids, Destination),
  [{graph_pid, G}] = ets:lookup(interscsimulator, graph_pid),
  Path = ets:lookup(bfs_cache, {Origin, Destination}),

  S2 = case Path of
    [] ->
      NewPath = digraph:get_short_path(G, U, V),
      case NewPath of
        false ->
          print_error("No path is possible from ~p to ~p~n", [Origin, Destination]),
          class_Actor:send_actor_message(WhoPid, {update_path, error}, State);
        _ ->
          print_error("Solving from (~p)->(~p).", [Origin, Destination]),
          ets:insert(bfs_cache, {{Origin, Destination}, NewPath}),
          class_Actor:send_actor_message(WhoPid, {update_path, [NewPath]}, State)
      end;
    [{{Origin, Destination}, NewPath}] ->
      class_Actor:send_actor_message(WhoPid, {update_path, [NewPath]}, State)
  end,
  ?wooper_return_state_only(S2).

update_capacity(State, {V1Idx, V2Idx, Factor}, _WhoPid) ->
  [{{V1Idx, V2Idx}, E}] = ets:lookup(edges_pids, {V1Idx, V2Idx}),
  [{graph_pid, G}] = ets:lookup(interscsimulator, graph_pid),
  {E, V1_vtx, V2_vtx, Label} = digraph:edge(G, E),
  {X1, X2, X3, Capacity, X5, X6, X7} = Label,
  NewLabel = {X1, X2, X3, Capacity + Factor, X5, X6, X7},
  digraph:add_edge(G, E, V1_vtx, V2_vtx, NewLabel),
	?wooper_return_state_only(State).

send_me_ets(State, EtsTabl, WhoPid) ->
  print_info("send_me_ets ~p call received!", [EtsTabl]),
  EtsTablContent = ets:match_object(EtsTabl, {'$0', '$1'}),
  class_Actor:send_actor_message(WhoPid, {update_your_ets, {EtsTabl, EtsTablContent}}, State).

serialize_digraph({digraph, V, E, N, B}) ->
    {ets:tab2list(V),
     ets:tab2list(E),
     ets:tab2list(N), B}.

send_me_digraph(State, _, WhoPid) ->
  print_info("Lookup on graph_pid on node ~p.", [node()]),
  G = getAttribute(State, erl_graph_pid),
  Payload = zlib:gzip(term_to_binary(serialize_digraph(G))),
  class_Actor:send_actor_message(WhoPid, {update_your_digraph, {Payload}}, State).

add_to_paths_to_solve(State, {U, V}, _WhoPid) ->
  ets:insert(paths_to_solve, {{U, V}, ok}),
  ?wooper_return_state_only(State).

actSpontaneous(State) ->
  1 = class_Actor:get_current_tick_offset(State), % additional check
  resolve_needed_paths(),
  ?wooper_return_state_only(State).

resolve_needed_paths() ->
  Paths = ets:tab2list(paths_to_solve),
  do_resolve_needed_paths(Paths).

do_resolve_needed_paths(Paths) ->
  lists:foreach(fun({{U, V}, ok}) -> resolve_path({U, V}) end, Paths).

resolve_path({Origin, Destination}) ->
  [{Origin, U}] = ets:lookup(nodes_pids, Origin),
  [{Destination, V}] = ets:lookup(nodes_pids, Destination),
  [{graph_pid, G}] = ets:lookup(interscsimulator, graph_pid),
  Path = digraph:get_short_path(G, U, V),
  ets:insert(bfs_cache, {{Origin, Destination}, Path}).
