-record(node_state, {
    id :: integer(),
    m_bits :: integer(),
    self_ref :: gleam@erlang@process:subject(chord_msgs:msg()),
    stats_actor :: gleam@erlang@process:subject(chord_stats:stats_message()),
    successor :: gleam@erlang@process:subject(chord_msgs:msg()),
    successor_id :: integer(),
    predecessor :: gleam@option:option(gleam@erlang@process:subject(chord_msgs:msg())),
    predecessor_id :: gleam@option:option(integer()),
    finger_table :: list(chord_node:finger_entry()),
    storage :: gleam@dict:dict(integer(), binary()),
    pending_lookups :: integer()
}).
