-module(config_parser).
-include_lib("xmerl/include/xmerl.hrl").

-export([load_config_from_xml/1]).

load_config_from_xml(Filename) ->
  {Xml, _Misc} = xmerl_scan:file(Filename),
  SimplifiedXml = xmerl_lib:simplify_element(Xml),
  {_, _, RootValue} = SimplifiedXml,
  [_, InnerElement, _] = RootValue,
  {config, ConfigElement, _} = InnerElement,
  {
    extract_config_value(ConfigElement, output_file),
    extract_config_value(ConfigElement, simulation_time),
    extract_config_value(ConfigElement, map_file),
    extract_config_value(ConfigElement, trip_file),
    extract_config_value(ConfigElement, metro_file),
    extract_config_value(ConfigElement, bus_file),
    extract_config_value(ConfigElement, park_file),
    extract_config_value(ConfigElement, events_file),
    extract_config_value(ConfigElement, generate_graph)
  }.

extract_config_value([], _DesiredConfigValue) ->
  ok;
extract_config_value([H|T], DesiredConfigValue) ->
  {TagName, TagValue} = H,
  case TagName of
    DesiredConfigValue ->
      TagValue;
    _ ->
      extract_config_value(T, DesiredConfigValue)
  end.
