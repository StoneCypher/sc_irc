
-module(sc_irc_cmd).

% does not implement sending for SERVER message in 4.1.4, SQUIT 4.1.7 because client libs don't need them
% does not implement multiple join notation from 4.2.1, part 4.2.2, names 4.2.5, list 4.2.6 because who cares

% TODO whargarbl gotta walk section 4 again for parsing





-export([

    assemble/1,
      assemble/2,
      assemble/3,

    valid_assemble/2,
      valid_assemble/3,

    pass/1,
    nick/1,
    user/4,
    oper/2,
    quit/0,    quit/1,
    join/1,
    part/1,    part/2,
    mode/2,    mode/3,
    topic/1,   topic/2,
    names/0,   names/1,
    list/0,    list/1,
    invite/2,
    kick/2,    kick/3,
    version/0

]).





%% @doc Utility that assembles the given arguments blindly into a command string without validating their legality.

assemble(Command)                   -> Command.
assemble(Command, Params)           -> sc:implode(" ", [Command] ++ Params).
assemble(Command, Params, Trailing) -> sc:implode(" ", [Command] ++ Params ++ [":" ++ Trailing]).





%% @doc Utility that validates the command and params, then assembles them in standard command format.  Throws on bad param or command.

valid_assemble(Command, Params) -> 

    sc_irc_validate:paramlist(Params),
    assemble(Command, Params).





%% @doc Utility that validates the command, params, and trailing string, then assembles them in standard command format.  Throws on bad command, param, or trailing string.

valid_assemble(Command, Params, Trailing) -> 

    sc_irc_validate:param(Command),  % todo *should* test against the complete list of commands instead whargarbl
    sc_irc_validate:paramlist_and_trailing(Params, Trailing),
    assemble(Command, Params, Trailing).





%% @doc Sets the connection password at the beginning of the connection sequence.

pass(Password) when is_list(Password) ->

    valid_assemble("PASS", [Password]).

    % expect ERR_NEEDMOREPARAMS | ERR_ALREADYREGISTRED





%% @doc Sets a client nickname.  TODO whargarbl how do I best handle setting this to max 9 chars when in reality that's rarely the case?

nick(NewNick) when is_list(NewNick) ->

    valid_assemble("NICK", [NewNick]).

    % expect ERR_NONICKNAMEGIVEN | ERR_ERRONEUSNICKNAME | ERR_NICKNAMEINUSE | ERR_NICKCOLLISION





%% @doc Sets a user account with username, hostname, and servername as parameters, then with realname as a trailing string.

user(UserName, HostName, ServerName, RealName) when is_list(UserName), is_list(HostName), is_list(ServerName), is_list(RealName) ->

    valid_assemble("USER", [UserName, HostName, ServerName], RealName).

    % expect ERR_NONICKNAMEGIVEN | ERR_ERRONEUSNICKNAME | ERR_NICKNAMEINUSE | ERR_NICKCOLLISION





%% @doc Requests oper status with a username and password.

oper(UserName, Password) when is_list(UserName), is_list(Password) ->

    valid_assemble("OPER", [UserName, Password]).

    % expect ERR_NEEDMOREPARAMS | RPL_YOUREOPER | ERR_NOOPERHOST | ERR_PASSWDMISMATCH





%% @doc Terminates the connection with the standard quitmessage "sc_irc".

quit() ->

    quit("sc_irc").

    % expect (nothing)





%% @doc Terminates the connection with a denoument.

quit(QuitMessage) when is_list(QuitMessage) ->

    valid_assemble("QUIT", [], QuitMessage).

    % expect (nothing)





%% @doc Joins a channel; must include # or &amp; sigil

join(ChannelName) when is_list(ChannelName) ->

    sc_irc_validate:channel_name(ChannelName),
    valid_assemble("JOIN", [ChannelName]).

    % expect ERR_NEEDMOREPARAMS | ERR_BANNEDFROMCHAN | ERR_INVITEONLYCHAN | ERR_BADCHANNELKEY 
    %      | ERR_CHANNELISFULL  | ERR_BADCHANMASK    | ERR_NOSUCHCHANNEL  | ERR_TOOMANYCHANNELS 
    %      | RPL_TOPIC





%% @doc Departs a channel with the standard partmessage "sc_irc"

part(ChannelName) when is_list(ChannelName) ->

    part(ChannelName, "sc_irc").





%% @doc Departs a channel; must include # or &amp; sigil.  Standard does not seem to accomodate
%% a part message, but as it's a standard piece of IRC, it's being supported here.

part(ChannelName, PartMessage) when is_list(ChannelName), is_list(PartMessage) ->

    sc_irc_validate:channel_name(ChannelName),
    valid_assemble("JOIN", [ChannelName], PartMessage).

    % expect ERR_NEEDMOREPARAMS | ERR_NOSUCHCHANNEL | ERR_NOTONCHANNEL





%% @doc Set a user or a channel mode.

mode(ChannelOrUser, ModeString) when is_list(ChannelOrUser), is_list(ModeString) ->

    sc_irc_validate:channel_or_user(ChannelOrUser),
    valid_assemble("MODE", [ChannelOrUser, ModeString]).

    % expect ERR_NEEDMOREPARAMS | RPL_CHANNELMODEIS | ERR_CHANOPRIVSNEEDED | ERR_NOSUCHNICK
    %        ERR_NOTONCHANNEL   | ERR_KEYSET        | RPL_BANLIST          | RPL_ENDOFBANLIST
    %        ERR_UNKNOWNMODE    | ERR_NOSUCHCHANNEL | ERR_USERSDONTMATCH   | RPL_UMODEIS
    %        ERR_UMODEUNKNOWNFLAG





%% @doc Set a user or a channel mode that takes an argument (like a ban mask or a channel size.)

mode(ChannelOrUser, ModeString, Arg) when is_list(ChannelOrUser), is_list(ModeString), is_list(Arg) ->

    sc_irc_validate:channel_or_user(ChannelOrUser),
    valid_assemble("MODE", [ChannelOrUser, ModeString, Arg]).

    % expect ERR_NEEDMOREPARAMS | RPL_CHANNELMODEIS | ERR_CHANOPRIVSNEEDED | ERR_NOSUCHNICK
    %        ERR_NOTONCHANNEL   | ERR_KEYSET        | RPL_BANLIST          | RPL_ENDOFBANLIST
    %        ERR_UNKNOWNMODE    | ERR_NOSUCHCHANNEL | ERR_USERSDONTMATCH   | RPL_UMODEIS
    %        ERR_UMODEUNKNOWNFLAG





%% @doc Gets a channel's topic.

topic(ChannelName) when is_list(ChannelName) ->

    sc_irc_validate:channel_name(ChannelName),
    valid_assemble("TOPIC", [ChannelName]).

    % expect ERR_NOTONCHANNEL | RPL_NOTOPIC | RPL_TOPIC | ERR_CHANOPRIVSNEEDED





%% @doc Sets a channel topic.

topic(ChannelName, NewTopic) when is_list(ChannelName), is_list(NewTopic) ->

    sc_irc_validate:channel_name(ChannelName),
    valid_assemble("TOPIC", [ChannelName], NewTopic).

    % expect ERR_NEEDMOREPARAMS | ERR_NOTONCHANNEL | RPL_TOPIC | ERR_CHANOPRIVSNEEDED





%% @doc Lists every visible user and every visible channel (not generally a good idea.)

names() ->

    valid_assemble("NAMES", []).

    % expect RPL_NAMREPLY | RPL_ENDOFNAMES





%% @doc Lists all visible users in a single channel.

names(ChannelName) when is_list(ChannelName) ->

    sc_irc_validate:channel_name(ChannelName),
    valid_assemble("NAMES", [ChannelName]).

    % expect RPL_NAMREPLY | RPL_ENDOFNAMES





%% @doc Lists visible channels and their topics.

list() ->

    valid_assemble("LIST", []).

    % expect ERR_NOSUCHSERVER | RPL_LISTSTART | RPL_LIST | RPL_LISTEND





%% @doc Lists the topic of a single channel.

list(ChannelName) when is_list(ChannelName) ->

    sc_irc_validate:channel_name(ChannelName),
    valid_assemble("LIST", [ChannelName]).

    % expect ERR_NOSUCHSERVER | RPL_LISTSTART | RPL_LIST | RPL_LISTEND





%% @doc Invites a user to a channel.

invite(UserName, ChannelName) when is_list(UserName), is_list(ChannelName) ->

    sc_irc_validate:user_name(UserName),
    sc_irc_validate:channel_name(ChannelName),
    valid_assemble("INVITE", [UserName, ChannelName]).

    % expect ERR_NEEDMOREPARAMS   | ERR_NOSUCHNICK | ERR_NOTONCHANNEL | ERR_USERONCHANNEL
    %        ERR_CHANOPRIVSNEEDED | RPL_INVITING   | RPL_AWAY





%% @doc Forcibly removes a user from a channel.

kick(ChannelName, UserName) when is_list(ChannelName), is_list(UserName) ->

    sc_irc_validate:channel_name(ChannelName),
    sc_irc_validate:user_name(UserName),
    valid_assemble("KICK", [UserName, ChannelName]).

    % expect ERR_NEEDMOREPARAMS | ERR_NOSUCHCHANNEL | ERR_BADCHANMASK | ERR_CHANOPRIVSNEEDED | ERR_NOTONCHANNEL





%% @doc Forcibly removes a user from a channel, with a reason message.

kick(ChannelName, UserName, Reason) when is_list(ChannelName), is_list(UserName), is_list(Reason) ->

    sc_irc_validate:channel_name(ChannelName),
    sc_irc_validate:user_name(UserName),
    valid_assemble("KICK", [UserName, ChannelName], Reason).

    % expect ERR_NEEDMOREPARAMS | ERR_NOSUCHCHANNEL | ERR_BADCHANMASK | ERR_CHANOPRIVSNEEDED | ERR_NOTONCHANNEL





%% @doc Gets the version of the IRC server software this client is connected directly to.

version() ->

    valid_assemble("VERSION", []).

    % expect ERR_NEEDMOREPARAMS | ERR_NOSUCHCHANNEL | ERR_BADCHANMASK | ERR_CHANOPRIVSNEEDED | ERR_NOTONCHANNEL
