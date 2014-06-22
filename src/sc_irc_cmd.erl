
-module(sc_irc_cmd).





-export([

    pass/1

]).





pass(Password) when is_list(Password) ->

    sc_irc_util:throw_on_illegal_param(Password),

    "PASS " ++ Passwd.

    % expect ERR_NEEDMOREPARAMS or ERR_ALREADYREGISTRED