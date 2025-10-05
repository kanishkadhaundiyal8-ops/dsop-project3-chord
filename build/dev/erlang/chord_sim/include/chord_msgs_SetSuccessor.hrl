-record(set_successor, {
    key :: integer(),
    successor_ref :: gleam@erlang@process:subject(chord_msgs:msg()),
    successor_id :: integer(),
    hop_count :: integer()
}).
