
-module(sc_irc_cmd).





-export([

    pass/1,
    nick/1,
    user/4

]).





pass(Password) when is_list(Password) ->

    sc_irc_util:throw_on_illegal_param(Password),

    "PASS " ++ Password.

    % expect ERR_NEEDMOREPARAMS | ERR_ALREADYREGISTRED





nick(NewNick) when is_list(NewNick) ->

    sc_irc_util:throw_on_illegal_param(NewNick),

    "NICK " ++ NewNick.

    % expect ERR_NONICKNAMEGIVEN | ERR_ERRONEUSNICKNAME | ERR_NICKNAMEINUSE | ERR_NICKCOLLISION





user(UserName, HostName, ServerName, RealName) when is_list(UserName), is_list(HostName), is_list(ServerName), is_list(RealName) ->

    sc_irc_util:throw_on_illegal_paramlist_and_trailing( [UserName, HostName, ServerName], RealName),

    "USER " ++ UserName
     ++ " " ++ HostName
     ++ " " ++ ServerName
    ++ " :" ++ RealName.

    % expect ERR_NONICKNAMEGIVEN | ERR_ERRONEUSNICKNAME | ERR_NICKNAMEINUSE | ERR_NICKCOLLISION
