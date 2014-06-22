
-module(sc_irc_cmd).





-export([

    pass/1

]).





pass(Passwd) when is_list(Passwd) ->

    "PASS " ++ Passwd