-record(statistics_state, {
    hop_sum :: integer(),
    finished_node_count :: integer(),
    total_nodes :: integer(),
    requests_per_node :: integer(),
    main_process :: gleam@erlang@process:subject(chord_stats:stats_message())
}).
