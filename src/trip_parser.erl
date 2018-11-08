-module(trip_parser).

-include_lib("xmerl/include/xmerl.hrl").

-export([load_trips_from_xml/1]).


load_trips_from_xml(Filename) ->
  {Xml, _Misc} = xmerl_scan:file(Filename),
  SimplifiedXml = xmerl_lib:simplify_element(Xml),
  {scsimulator_matrix, _, InnerElement} = SimplifiedXml,
  do_parse_trips(InnerElement, []).

do_parse_trips([], Agg) ->
  Agg;
do_parse_trips([H|T], Agg) ->
  case H of
    {trip, Attrs, _} ->
      Trip = mount_trip(Attrs),
      do_parse_trips(T, [Trip | Agg]);
    _ ->
      do_parse_trips(T, Agg)
  end.

mount_trip(A) ->
  {
    look_for_attr(A, origin),
    look_for_attr(A, destination),
    look_for_attr(A, count),
    look_for_attr(A, start),
    look_for_attr(A, link_origin),
    look_for_attr(A, type),
    look_for_attr(A, mode),
    look_for_attr(A, name),
    look_for_attr(A, park),
    look_for_attr(A, uuid)
  }.

look_for_attr(Available, Desired) ->
  case lists:keyfind(Desired, 1, Available) of
    {Desired, V} ->
      V;
    _ ->
      io:format("~n[ERROR] Key `~p` not found in trip attributes list!~n", [Desired]),
      throw({key_not_found, Desired})
  end.
