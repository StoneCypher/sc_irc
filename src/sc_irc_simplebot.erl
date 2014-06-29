
-module(sc_irc_simplebot).





-export([

    start/0,
    stop/1,

    is_simplebot/1,

    connect_to/2

]).





is_simplebot( { sc_irc_simplebot, _ } ) -> true;
is_simplebot( _ )                       -> false.





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
