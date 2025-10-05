-module(chord_msgs).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\chord_msgs.gleam").
-export_type([msg/0]).

-type msg() :: {request_join, gleam@erlang@process:subject(msg())} |
    {set_successor,
        integer(),
        gleam@erlang@process:subject(msg()),
        integer(),
        integer()} |
    {find_successor_for,
        integer(),
        gleam@erlang@process:subject(msg()),
        integer()} |
    run_stabilization |
    {fetch_predecessor, gleam@erlang@process:subject(msg())} |
    {return_predecessor,
        gleam@option:option(gleam@erlang@process:subject(msg())),
        gleam@option:option(integer())} |
    {announce, gleam@erlang@process:subject(msg()), integer()} |
    {update_fingers, integer()} |
    verify_predecessor |
    {start_lookups, integer()} |
    {find_key, integer(), integer()} |
    heartbeat.


