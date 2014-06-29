
-module(sc_irc_simplebot).





-export([

    start/0,
    stop/1

]).





loop() ->

    receive

        terminate ->
            ok;

        _ ->
            loop()

    end.





start() ->

    { sc_irc_simplebot, spawn( fun() -> loop() end ) }.






stop( { sc_irc_simplebot, Pid } ) ->

    Pid ! terminate.
