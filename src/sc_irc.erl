
-module(sc_irc).





-export( [

	connect/1,
	disconnect/1,

	channel_type/1,
	  legal_channel_name_char/1

] ).





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





%% @doc Predicate returning true for characters that are legal within a channel name.

legal_channel_name_char(7)  -> false;
legal_channel_name_char($ ) -> false;
legal_channel_name_char($,) -> false;
legal_channel_name_char(I) when is_integer(I), I >= 0, I =< 255 -> true.  % whargarbl can this take nulls and etc?  what's the charset
