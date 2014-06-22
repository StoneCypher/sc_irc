
-module(sc_irc_util).





-export([

    irc_strcomp_i/2,
      irc_strcomp_i_nordfix/1

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
