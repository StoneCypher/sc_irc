
-module(sc_irc_cmd).





-export([

    pass/1

]).





pass(Password) when is_list(Password) ->

    sc_irc_util:throw_on_illegal_param(Password),

    "PASS " ++ Passwd.

    % expect ERR_NEEDMOREPARAMS | ERR_ALREADYREGISTRED





nick(NewNick) when is_list(Nick) ->

    sc_irc_util:throw_on_illegal_param(Nick),

    "NICK " ++ Passwd.

    % expect ERR_NONICKNAMEGIVEN | ERR_ERRONEUSNICKNAME | ERR_NICKNAMEINUSE | ERR_NICKCOLLISION
