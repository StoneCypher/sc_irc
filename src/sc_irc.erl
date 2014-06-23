
%% @doc sc_irc - a simple RFC 1459 client for Erlang
%%
%% http://irchelp.org/irchelp/rfc/rfc.html


%% @todo max nick length semi-configurable - 9, any, or server-defined, rfc section 1.2 vs reality
%% @todo ircops mentioned section 1.2.1; squit, connect, kill
%% @todo 1.3.1 chanops can kick, mode, invite, topic
%% @todo section 2.3.1 parsing grammar
%% @todo section 2.4 reply requirements
%% @todo section 2.5 wildcards (wut)
%% @todo section 3 response on no such server
%% @todo section 3.1 PASS and SERVICE connections





-module(sc_irc).





-export( [

    connect/1,
      connect/2,

    disconnect/1,

    channel_type/1,
      legal_channel_name_char/1,

    parse_message/1

] ).






-record( irc_message, {

    prefix     = none,
    command,
    parameters  = [],
    paramstring = ""

} ).





%% @doc Connect to a server.

connect(_Config) ->

    todo.





%% @doc Connect to a server.

connect(ManagedConfig, Handler) ->

    todo.





%% @doc Disconnect from a server.

disconnect(_Pid) ->

    todo.





%% @doc Describes a channel by type (local or global,) or throws if not a valid channel name.  To be valid, a channel must start with # or &amp;,
%% must not contain a space, a control-g, a comma, and must be 200 or fewer characters including the sigil.  The standard is vague on whether
%% just-the-sigil is legitimate, so this library tolerates it.

-spec channel_type(ChannelName::[char()]) -> local | global.

% 1.3 defines the channel length as up to 200 including prefix
% todo spec tests around length

channel_type(Name) when is_list(Name), length(Name) > 200 ->

    throw(badarg);



channel_type([$&|Rem]) ->

    case lists:all(fun legal_channel_name_char/1, Rem) of
        true -> local;
        false -> throw(badarg)
    end;



channel_type([$#|Rem]) ->

    case lists:all(fun legal_channel_name_char/1, Rem) of
        true -> global;
        false -> throw(badarg)
    end.





%% @doc Predicate returning true for characters that are legal within a channel name.  Spec section `1.3'.

legal_channel_name_char(7)  -> false;
legal_channel_name_char($ ) -> false;
legal_channel_name_char($,) -> false;
legal_channel_name_char(I) when is_integer(I), I >= 0, I =< 255 -> true.  % whargarbl can this take nulls and etc?  what's the charset





%% @doc Parses a message received from an IRC server.  Spec section `2.3'.

parse_message([$:|RemMessage]) ->

    [ Front, MaybeTail ] = sc:explode(" :", RemMessage, 2),

    [Prefix, Command | Parameters] = sc:explode(" ", Front),
    #irc_message{ prefix=Prefix, command=Command, parameters=Parameters ++ [MaybeTail] };





parse_message(Message) when is_list(Message) ->

    [ Front, MaybeTail ] = sc:explode(" :", Message, 2),

    [Command | Parameters] = sc:explode(" ", Front),
    #irc_message{ command=Command, parameters=Parameters ++ [MaybeTail] }.
