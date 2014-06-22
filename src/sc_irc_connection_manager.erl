
-module(sc_irc_connection_manager).





-include("sc_irc.hrl").





-export([

    open/2,
      open/3,

    close/1

]).





construct_loop(Config, Handler, ConnectionTag) ->

    NewConfig  = #connection_config{ server=Config#managed_connection_config.server, port=Config#managed_connection_config.port },
    Connection = sc_irc_connection:open( NewConfig, self() ),

    loop(Config, Handler, ConnectionTag, Connection).





loop(Config, Handler, ConnectionTag, Connection) ->

    receive

        terminate ->
            sc_irc_connection:close(Connection),
            ok;

% todo

        MisunderstoodMessage ->
            io:format("~p~n~n", [MisunderstoodMessage]), % todo
            Handler ! { error, sc_irc_connection_manager, misunderstood_message, MisunderstoodMessage },
            loop(Config, Handler, ConnectionTag, Connection)

    end.





open(Config, Handler) ->

    open(Config, Handler, no_tag).





open(Config, Handler, ConnectionTag) ->

    { sc_irc_connection_manager, connection_handle, spawn(fun() -> construct_loop(Config, Handler, ConnectionTag) end) }.





close(ConnectionHandle) ->

    todo.
