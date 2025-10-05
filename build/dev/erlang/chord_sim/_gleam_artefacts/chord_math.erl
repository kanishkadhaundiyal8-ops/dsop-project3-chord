-module(chord_math).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\chord_math.gleam").
-export([integer_power/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src\\chord_math.gleam", 6).
-spec power_recursive_helper(integer(), integer(), integer()) -> integer().
power_recursive_helper(Base, Exponent, Accumulator) ->
    case Exponent of
        0 ->
            Accumulator;

        _ ->
            power_recursive_helper(Base, Exponent - 1, Base * Accumulator)
    end.

-file("src\\chord_math.gleam", 2).
?DOC(" Calculates integer power using tail recursion for safety.\n").
-spec integer_power(integer(), integer()) -> integer().
integer_power(Base, Exponent) ->
    power_recursive_helper(Base, Exponent, 1).
