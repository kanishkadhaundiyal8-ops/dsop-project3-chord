-record(return_predecessor, {
    predecessor :: gleam@option:option(gleam@erlang@process:subject(chord_msgs:msg())),
    pred_id :: gleam@option:option(integer())
}).
