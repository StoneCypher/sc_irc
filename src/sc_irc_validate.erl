
-module(sc_irc_validate).





-export([

    is_legal_param/1,
      is_legal_trailing/1,

    param/1,
    paramlist/1,
    trailing/1,
    paramlist_and_trailing/2,
    channel_name/1

]).





is_legal_param(Param) ->

    true.  % todo whargarbl comeback





%% @doc Throws if the argument passed in isn't a valid IRC command parameter.

param(Param) ->

    case is_legal_param(Param) of
        false -> throw(badarg);
        _     -> ok
    end.





is_legal_trailing(Trailing) ->

    true.  % todo whargarbl comeback





%% @doc Throws if the argument passed in isn't a valid IRC command trailing string.

trailing(Trailing) ->

    case is_legal_trailing(Trailing) of
        false -> throw(badarg);
        _     -> ok
    end.





%% @doc Throws if the argument passed in isn't a list of all valid IRC command parameters.

paramlist(ParamList) ->

    [ param(Auditables) || Auditables <- ParamList ].





%% @doc Throws if the first argument passed in isn't a list of all valid IRC command parameters, and/or the second a valid trailing string.

paramlist_and_trailing(ParamList, Trailing) ->

    paramlist(ParamList),
    trailing(Trailing).





%% @doc Throws if the argument passed in isn't a valid channel name (NOT IMPLEMENTED).

channel_name(CName) ->

    ok.





%% @doc Throws if the argument passed in isn't a valid channel name (NOT IMPLEMENTED).

user_name(CName) ->

    ok.





%% @doc Throws if the argument passed in isn't a valid channel name or username (NOT IMPLEMENTED).

channel_or_user(CName) ->

    ok.
