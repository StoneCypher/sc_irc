
-record(connection_config, {

    server,
    port = 6667

}).





-record(managed_connection_config, {

    server,
    port                 = 6667,

    nick,
    pass,

    user_name            = "sc_irc",
    host_name            = "8",                           % irc legacy - 0 for visible, 8 for invisible
    server_name          = "*",                           % irc legacy - now meaningless; always use *
    real_name            = "sc_irc connection_manager",

    auto_ping_pong       = true,

    auto_reconnect       = true,
    reconnect_max_tries  = 99,
    reconnect_delay_msec = 5000

}).
