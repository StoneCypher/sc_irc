
-module(sc_irc_connection_manager).





-include("sc_irc.hrl").





-export([

    open/2,
      open/3,

    close/1

]).





construct_loop(Config, Handler, ConnectionTag) ->

    ConnectionConfig = #connection_config{ server=Config#managed_connection_config.server, port=Config#managed_connection_config.port },
    Connection       = sc_irc_connection:open( ConnectionConfig, self() ),

    sc_irc_connection:send( Connection, sc_irc_cmd:pass(Config#managed_connection_config.pass) ),
    sc_irc_connection:send( Connection, sc_irc_cmd:nick(Config#managed_connection_config.nick) ),
    sc_irc_connection:send( Connection, sc_irc_cmd:user(Config#managed_connection_config.user_name, Config#managed_connection_config.real_name) ),

    loop(Config, Handler, ConnectionTag, Connection, Config#managed_connection_config.auto_ping_pong).





loop(Config, Handler, ConnectionTag, Connection, AutoPing) ->

    receive

        terminate ->
            sc_irc_connection:close(Connection),
            ok;

        { error, connection_lost } ->
            io:format("whargarbl: should implement reconnect behavior then loop here~n~n", []), % todo
            { todo, whargarbl, "should implement reconnect behavior then loop here" };

        { irc_receive, {irc_message, _Server, "PING", [PingVal]}, ConnectionTag } when AutoPing == true ->
            sc_irc_connection:send( Connection, sc_irc_cmd:pong(PingVal) ),
            loop(Config, Handler, ConnectionTag, Connection, AutoPing);

        { irc_receive, Message, ConnectionTag } ->
            io:format("[Received] ~p~n~n", [Message]), % todo
            Handler ! { irc_receive, Message, ConnectionTag },
            loop(Config, Handler, ConnectionTag, Connection, AutoPing);

        MisunderstoodMessage ->
            io:format("[Mgr Misunderstood] ~p~n~n", [MisunderstoodMessage]), % todo
            Handler ! { error, sc_irc_connection_manager, misunderstood_message, MisunderstoodMessage },
            loop(Config, Handler, ConnectionTag, Connection, AutoPing)

    end.





open(Config, Handler) ->

    open(Config, Handler, no_tag).





open(Config, Handler, ConnectionTag) ->

    { sc_irc_connection_manager, connection_handle, spawn(fun() -> construct_loop(Config, Handler, ConnectionTag) end) }.





close(_ConnectionHandle) ->

    todo.
