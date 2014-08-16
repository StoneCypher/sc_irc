
-record(connection_config, {

    server      :: list(),
    port = 6667 :: non_neg_integer()

}).





-record(managed_connection_config, {

    server                                             :: list(),
    port                 = 6667                        :: non_neg_integer(),

    nick                                               :: list(),
    pass                                               :: list(),

    user_name            = "sc_irc"                    :: list(),
    host_name            = "8"                         :: list(),   % irc legacy - 0 for visible, 8 for invisible
    server_name          = "*"                         :: list(),   % irc legacy - now meaningless; always use *
    real_name            = "sc_irc connection_manager" :: list(),

    auto_ping_pong       = true                        :: boolean(),

    auto_reconnect       = true                        :: boolean(),
    reconnect_max_tries  = 99                          :: non_neg_integer(),
    reconnect_delay_msec = 5000                        :: non_neg_integer()

}).
