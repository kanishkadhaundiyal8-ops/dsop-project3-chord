-record(find_successor_for, {
    key :: integer(),
    reply_to :: gleam@erlang@process:subject(chord_msgs:msg()),
    hop_count :: integer()
}).
