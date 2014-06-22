
-module(sc_irc_validate).





-export([

    is_legal_param/1,
      is_legal_trailing/1,

    param/1,
    paramlist/1,
    trailing/1,
    paramlist_and_trailing/2,
    channel_name/1,
    user_name/1,
    channel_or_user/1

]).





throw_if_not(false) -> throw(badarg);
throw_if_not( _ )   -> ok.





is_legal_param(_Param) ->

    true.  % todo whargarbl comeback





is_legal_trailing(_Trailing) ->

    true.  % todo whargarbl comeback





is_legal_user_name(_UserName) ->

    true.  % todo whargarbl comeback





is_legal_channel_name(_ChannelName) ->

    true.  % todo whargarbl comeback





%% @doc Throws if the argument passed in isn't a valid IRC command parameter.

param(Param) ->

    throw_if_not( is_legal_param(Param) ).





%% @doc Throws if the argument passed in isn't a valid IRC command trailing string.

trailing(Trailing) ->

    throw_if_not( is_legal_trailing(Trailing) ).





%% @doc Throws if the argument passed in isn't a list of all valid IRC command parameters.

paramlist(ParamList) ->

    [ param(Auditables) || Auditables <- ParamList ].





%% @doc Throws if the first argument passed in isn't a list of all valid IRC command parameters, and/or the second a valid trailing string.

paramlist_and_trailing(ParamList, Trailing) ->

    paramlist(ParamList),
    trailing(Trailing).





%% @doc Throws if the argument passed in isn't a valid channel name (NOT IMPLEMENTED).

channel_name(CName) ->

    throw_if_not( is_legal_channel_name(CName) ).





%% @doc Throws if the argument passed in isn't a valid channel name (NOT IMPLEMENTED).

user_name(UName) ->

    throw_if_not( is_legal_user_name(UName) ).





%% @doc Throws if the argument passed in isn't a valid channel name or username (NOT IMPLEMENTED).

channel_or_user(CUName) ->

    throw_if_not( is_legal_user_name(CUName) orelse is_legal_channel_name(CUName) ).
