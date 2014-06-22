
-module(sc_irc_cmd).

% does not implement SERVER message in 4.1.4 because client libs don't need that





-export([

    assemble/1,
      assemble/2,
      assemble/3,

    valid_assemble/2,
      valid_assemble/3,

    pass/1,
    nick/1,
    user/4,
    oper/2

]).





%% @doc Utility that assembles the given arguments blindly into a command string without validating their legality.

assemble(Command)                   -> Command.
assemble(Command, Params)           -> sc:implode(" ", [Command] ++ Params).
assemble(Command, Params, Trailing) -> sc:implode(" ", [Command] ++ Params ++ [":" ++ Trailing]).





%% @doc Utility that validates the command and params, then assembles them in standard command format.  Throws on bad param or command.

valid_assemble(Command, Params) -> 

    sc_irc_util:throw_on_illegal_paramlist(Params),
    assemble(Command, Params).





%% @doc Utility that validates the command, params, and trailing string, then assembles them in standard command format.  Throws on bad command, param, or trailing string.

valid_assemble(Command, Params, Trailing) -> 

    sc_irc_util:throw_on_illegal_param(Command),  % todo *should* test against the complete list of commands instead whargarbl
    sc_irc_util:throw_on_illegal_paramlist_and_trailing(Params, Trailing),
    assemble(Command, Params, Trailing).





%% @doc Renders the command string to set a connection password at the beginning of the connection sequence.

pass(Password) when is_list(Password) ->

    sc_irc_util:valid_assemble("PASS", [Password]).

    % expect ERR_NEEDMOREPARAMS | ERR_ALREADYREGISTRED





%% @doc Renders the command string to set a nickname.  TODO whargarbl how do I best handle setting this to max 9 chars when in reality that's rarely the case?

nick(NewNick) when is_list(NewNick) ->

    sc_irc_util:valid_assemble("NICK", [NewNick]).

    % expect ERR_NONICKNAMEGIVEN | ERR_ERRONEUSNICKNAME | ERR_NICKNAMEINUSE | ERR_NICKCOLLISION





%% @doc Renders the command string to set a user account with username, hostname, and servername as parameters, then with realname as a trailing string.

user(UserName, HostName, ServerName, RealName) when is_list(UserName), is_list(HostName), is_list(ServerName), is_list(RealName) ->

    valid_assemble("USER", [UserName, HostName, ServerName], RealName).

    % expect ERR_NONICKNAMEGIVEN | ERR_ERRONEUSNICKNAME | ERR_NICKNAMEINUSE | ERR_NICKCOLLISION





%% @doc Renders the command string to request oper status with a username and password.

oper(UserName, Password) when is_list(UserName), is_list(Password) ->

    valid_assemble("OPER", [UserName, Password]).

    % expect ERR_NEEDMOREPARAMS | RPL_YOUREOPER | ERR_NOOPERHOST | ERR_PASSWDMISMATCH
