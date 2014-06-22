
%% @doc sc_irc - a simple RFC 1459 client for Erlang
%%
%% http://irchelp.org/irchelp/rfc/rfc.html


%% @todo max nick length semi-configurable - 9, any, or server-defined, rfc section 1.2 vs reality
%% @todo ircops mentioned section 1.2.1; squit, connect, kill
%% @todo 1.3.1 chanops can kick, mode, invite, topic






-module(sc_irc).





-export( [

	connect/1,
	disconnect/1,

    irc_strcomp_i/2,
      irc_strcomp_i_nordfix/1,

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





%% @doc Disconnect from a server.

disconnect(_Pid) ->

    todo.





%% @doc Describes a channel by type (local or global,) or throws if not a valid channel name.

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

    [Prefix, Command | Parameters] = sc:explode(" ", RemMessage),
    #irc_message{ prefix=Prefix, command=Command, parameters=Parameters, paramstring=sc:implode(" ", Parameters) };





parse_message(Message) when is_list(Message) ->

    [Command | Parameters] = sc:explode(" ", Message),
    #irc_message{ command=Command, parameters=Parameters, paramstring=sc:implode(" ", Parameters) }.





%% @doc Destructively prepares a string for comparison in the IRC nordic characterset.  Throws away substantial information and returns the string reversed.

irc_strcomp_i_nordfix(String) ->

    irc_strcomp_i_nordfix(String, []).



irc_strcomp_i_nordfix([],         Work)                         -> Work;

irc_strcomp_i_nordfix([${ | Rem], Work)                         -> irc_strcomp_i_nordfix(Rem, [$[]             ++ Work);  % section 2.2, character codes
irc_strcomp_i_nordfix([$} | Rem], Work)                         -> irc_strcomp_i_nordfix(Rem, [$]]             ++ Work);  % {}| are upper case []\
irc_strcomp_i_nordfix([$| | Rem], Work)                         -> irc_strcomp_i_nordfix(Rem, [$\\]            ++ Work);  % http://irchelp.org/irchelp/rfc/chapter2.html
irc_strcomp_i_nordfix([Ch | Rem], Work) when Ch >= $a, Ch =< $z -> irc_strcomp_i_nordfix(Rem, [Ch - ($a - $A)] ++ Work);
irc_strcomp_i_nordfix([Ex | Rem], Work)                         -> irc_strcomp_i_nordfix(Rem, [Ex]             ++ Work).





%% @doc Case insensitively compares two strings given IRC's definition that `{}|' constitute lower-case `[]\\'.

irc_strcomp_i(Str1, Str2) ->

    string:equal( irc_strcomp_i_nordfix(Str1), irc_strcomp_i_nordfix(Str2) ).
