
-module(sc_irc_util).





-export([

    irc_strcomp_i/2,
      irc_strcomp_i_nordfix/1,

    is_legal_param/1,
      is_legal_trailing/1,

    throw_on_illegal_param/1,
      throw_on_illegal_trailing/1,
      throw_on_illegal_paramlist/1,
      throw_on_illegal_paramlist_and_trailing/2

]).





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





is_legal_param(Param) ->

    true.  % todo whargarbl comeback





%% @doc Throws if the argument passed in isn't a valid IRC command parameter.

throw_on_illegal_param(Param) ->

    case is_legal_param(Param) of
        false -> throw(badarg);
        _     -> ok
    end.





is_legal_trailing(Trailing) ->

    true.  % todo whargarbl comeback





%% @doc Throws if the argument passed in isn't a valid IRC command trailing string.

throw_on_illegal_trailing(Trailing) ->

    case is_legal_trailing(Trailing) of
        false -> throw(badarg);
        _     -> ok
    end.





%% @doc Throws if the argument passed in isn't a list of all valid IRC command parameters.

throw_on_illegal_paramlist(ParamList) ->

    [ throw_on_illegal_param(Auditables) || Auditables <- ParamList ].





%% @doc Throws if the first argument passed in isn't a list of all valid IRC command parameters, and/or the second a valid trailing string.

throw_on_illegal_paramlist_and_trailing(ParamList, Trailing) ->

    throw_on_illegal_paramlist(ParamList),
    throw_on_illegal_trailing(Trailing).
