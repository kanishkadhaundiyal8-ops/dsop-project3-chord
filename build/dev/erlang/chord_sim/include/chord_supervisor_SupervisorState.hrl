-record(supervisor_state, {
    node_count :: integer(),
    requests_per_node :: integer(),
    stats_actor :: gleam@erlang@process:subject(chord_stats:stats_message()),
    node_refs :: list(gleam@erlang@process:subject(chord_msgs:msg())),
    id_space_bits :: integer(),
    supervisor_subject :: gleam@erlang@process:subject(chord_supervisor:supervisor_msg())
}).
