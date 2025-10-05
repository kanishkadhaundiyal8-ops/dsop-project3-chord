-module(input).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).
-define(FILEPATH, "src/input.gleam").
-export([input/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " This package provides a single function, `input`, that prints a prompt\n"
    " and then reads a user's input. Inspired by the Python function of the\n"
    " same name.\n"
    " It is intended to work on erlang, node, deno, and bun.\n"
    " This package has no dependencies, not even the stdlib.\n"
).

-file("src/input.gleam", 9).
-spec input(binary()) -> {ok, binary()} | {error, nil}.
input(Prompt) ->
    input_ffi:input(Prompt).
