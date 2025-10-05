-module(chord_util).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\chord_util.gleam").
-export([map_to_id_space/2, compute_finger_start/3, generate_random_key/1, is_in_interval_inclusive/3, is_in_interval_exclusive/3]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src\\chord_util.gleam", 9).
?DOC(" Hashes an integer to fit within the Chord identifier space, [0, 2^m).\n").
-spec map_to_id_space(integer(), integer()) -> integer().
map_to_id_space(Value, M_bits) ->
    Ring_size = chord_math:integer_power(2, M_bits),
    case Ring_size of
        0 -> 0;
        Gleam@denominator -> gleam@int:absolute_value(Value) rem Gleam@denominator
    end.

-file("src\\chord_util.gleam", 16).
?DOC(
    " Calculates the starting identifier for a finger table entry.\n"
    " The i-th finger of node n is responsible for the key at (n + 2^(i-1)).\n"
).
-spec compute_finger_start(integer(), integer(), integer()) -> integer().
compute_finger_start(Node_id, Finger_index, M_bits) ->
    Ring_size = chord_math:integer_power(2, M_bits),
    Power_of_2 = chord_math:integer_power(2, Finger_index - 1),
    Start_id = case Ring_size of
        0 -> 0;
        Gleam@denominator -> (Node_id + Power_of_2) rem Gleam@denominator
    end,
    Start_id.

-file("src\\chord_util.gleam", 24).
?DOC(" Generates a random key within the identifier space [0, 2^m).\n").
-spec generate_random_key(integer()) -> integer().
generate_random_key(M_bits) ->
    Upper_bound = chord_math:integer_power(2, M_bits),
    gleam@int:random(Upper_bound).

-file("src\\chord_util.gleam", 31).
?DOC(
    " Checks if an ID is within the interval (start, end] on the Chord ring.\n"
    " Handles the wrap-around case where start > end.\n"
).
-spec is_in_interval_inclusive(integer(), integer(), integer()) -> boolean().
is_in_interval_inclusive(Id, Start, End_val) ->
    case Start < End_val of
        true ->
            (Id > Start) andalso (Id =< End_val);

        false ->
            (Id > Start) orelse (Id =< End_val)
    end.

-file("src\\chord_util.gleam", 42).
?DOC(
    " Checks if an ID is within the exclusive interval (start, end) on the Chord ring.\n"
    " Handles the wrap-around case.\n"
).
-spec is_in_interval_exclusive(integer(), integer(), integer()) -> boolean().
is_in_interval_exclusive(Id, Start, End_val) ->
    case Start < End_val of
        true ->
            (Id > Start) andalso (Id < End_val);

        false ->
            (Id > Start) orelse (Id < End_val)
    end.
