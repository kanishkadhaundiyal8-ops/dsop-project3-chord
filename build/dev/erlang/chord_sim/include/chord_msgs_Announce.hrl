-record(announce, {
    new_node :: gleam@erlang@process:subject(chord_msgs:msg()),
    new_node_id :: integer()
}).
