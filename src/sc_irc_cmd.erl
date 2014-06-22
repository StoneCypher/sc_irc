
-module(sc_irc_cmd).





-export([

    assemble/1,
      assemble/2,
      assemble/3,

    valid_assemble/2,
      valid_assemble/3,

    pass/1,
    nick/1,
    user/4

]).





assemble(Command)                   -> Command.
assemble(Command, Params)           -> sc:implode(" ", [Command] ++ Params).
assemble(Command, Params, Trailing) -> sc:implode(" ", [Command] ++ Params ++ [":" ++ Trailing]).





valid_assemble(Command, Params) -> 

    sc_irc_util:throw_on_illegal_paramlist(Params),
    assemble(Command, Params).





valid_assemble(Command, Params, Trailing) -> 

    sc_irc_util:throw_on_illegal_paramlist_and_trailing(Params, Trailing),
    assemble(Command, Params, Trailing).





pass(Password) when is_list(Password) ->

    sc_irc_util:throw_on_illegal_param(Password),

    "PASS " ++ Password.

    % expect ERR_NEEDMOREPARAMS | ERR_ALREADYREGISTRED





nick(NewNick) when is_list(NewNick) ->

    sc_irc_util:throw_on_illegal_param(NewNick),

    "NICK " ++ NewNick.

    % expect ERR_NONICKNAMEGIVEN | ERR_ERRONEUSNICKNAME | ERR_NICKNAMEINUSE | ERR_NICKCOLLISION





user(UserName, HostName, ServerName, RealName) when is_list(UserName), is_list(HostName), is_list(ServerName), is_list(RealName) ->

    valid_assemble("USER", [UserName, HostName, ServerName], RealName).

    % expect ERR_NONICKNAMEGIVEN | ERR_ERRONEUSNICKNAME | ERR_NICKNAMEINUSE | ERR_NICKCOLLISION
