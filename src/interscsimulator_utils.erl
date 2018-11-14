-module(interscsimulator_utils).

-export([print_info/1, print_info/2, print_error/1, print_error/2,
        print_success/1, print_success/2]).

print_info(Text) ->
  io:format("~n\033[94m[INFO]\033[0m ~p", [Text]).
print_info(Text, Args) ->
  io:format("~n\033[94m[INFO]\033[0m " ++ Text, Args).

print_error(Text) ->
  io:format("~n\033[31m[INFO]\033[0m ~p", [Text]).
print_error(Text, Args) ->
  io:format("~n\033[31m[INFO]\033[0m " ++ Text, Args).

print_success(Text) ->
  io:format("~n\033[32m[INFO]\033[0m ~p", [Text]).
print_success(Text, Args) ->
  io:format("~n\033[32m[INFO]\033[0m " ++ Text, Args).
