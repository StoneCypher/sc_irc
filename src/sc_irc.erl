
-module(sc_irc).





-export( [

	connect/1,
	disconnect/1,

	channel_type/1,
	  legal_channel_name_char/1,

	parse_message/1

] ).





-record( irc_message, {

    prefix     = none,
    command,
    parameters = []

} ).





%% @doc Connect to a server.

connect(_Config) ->

    todo.





%% @doc Disconnect from a server.

disconnect(_Pid) ->

    todo.





%% @doc Describes a channel by type (local or global,) or throws if not a valid channel name.

-spec channel_type(ChannelName::[char()]) -> local | global.

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

    [Prefix, Command | Parameters] = sc:explode(" ", RemMessage),
    #irc_message{ prefix=Prefix, command=Command, parameters=Parameters };





parse_message(Message) when is_list(Message) ->

    [Prefix, Command | Parameters] = sc:explode(" ", Message),
    #irc_message{ command=Command, parameters=Parameters }.
