-record(finger_entry, {
    start_id :: integer(),
    node_ref :: gleam@erlang@process:subject(chord_msgs:msg())
}).
