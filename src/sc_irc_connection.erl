
-module(sc_irc_connection).

-include("sc_irc.hrl").





-export([

    open/2,
      open/3,

    send/2,

    close/1

]).





construct_loop(Config, Handler, ConnectionTag) ->

    { ok, CSock } = gen_tcp:connect( Config#connection_config.server, Config#connection_config.port, [list] ),

    loop(Config, Handler, ConnectionTag, CSock, []).





dispatch_one(Handler, Message, ConnectionTag) when is_pid(Handler) ->

    Handler ! { irc_receive, Message, ConnectionTag };





dispatch_one(Handler, Message, ConnectionTag) when is_function(Handler) ->

    Handler(Message, ConnectionTag).





dispatch_complete_from([], _Handler, _ConnectionTag) ->

    [];





dispatch_complete_from(WorkQueue, Handler, ConnectionTag) ->

    { Keep, Parse } = case lists:last(WorkQueue) of

        $\r ->
            { [], string:tokens(WorkQueue, "\r\n") };

        $\n ->
            { [], string:tokens(WorkQueue, "\r\n") };

        _DoesNotEndInNewline ->
            [ RHead | RRem ] = lists:reverse(string:tokens(WorkQueue, "\r\n")),
            { RHead, lists:reverse(RRem) }

    end,

    [ dispatch_one(Handler, sc_irc:parse_message(Item), ConnectionTag) || Item <- Parse ],

    Keep.





loop(Config, Handler, ConnectionTag, Socket, WorkQueue) ->

    NextWorkQueue = dispatch_complete_from(WorkQueue, Handler, ConnectionTag),

    receive

        terminate ->
            gen_tcp:close(Socket),
            ok;

        { send_message, Message } ->
            gen_tcp:send(Socket, Message ++ "\r\n"),
            loop(Config, Handler, ConnectionTag, Socket, NextWorkQueue);

        { tcp, _, Data } ->
            loop(Config, Handler, ConnectionTag, Socket, NextWorkQueue ++ Data);

        { tcp_closed, _ } ->
            Handler ! { error, connection_lost },
            ok;

        MisunderstoodMessage ->
            Handler ! { error, sc_irc_connection, misunderstood_message, MisunderstoodMessage },
            loop(Config, Handler, ConnectionTag, Socket, NextWorkQueue)

    end.





open(Config, Handler) ->

    open(Config, Handler, no_tag).





open(Config, Handler, ConnectionTag) ->

    { sc_irc_connection, connection_handle, spawn(fun() -> construct_loop(Config, Handler, ConnectionTag) end) }.





close( { sc_irc_connection, connection_handle, ConnectionHandle } ) ->

    ConnectionHandle ! terminate.





send( { sc_irc_connection, connection_handle, ConnectionHandle }, Message ) ->

    ConnectionHandle ! { send_message, Message }.
