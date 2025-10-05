-module(main).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\main.gleam").
-export([main/0]).

-file("src\\main.gleam", 14).
-spec main() -> nil.
main() ->
    Nodes_str@1 = case input_ffi:input(<<"Enter the number of nodes: "/utf8>>) of
        {ok, Nodes_str} -> Nodes_str;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"main"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 16,
                        value => _assert_fail,
                        start => 329,
                        'end' => 400,
                        pattern_start => 340,
                        pattern_end => 353})
    end,
    Num_nodes = case gleam@int:base_parse(Nodes_str@1, 10) of
        {ok, Value} ->
            Value;

        {error, _} ->
            gleam_stdlib:println(
                <<"Invalid input. Using default: 50 nodes."/utf8>>
            ),
            50
    end,
    Reqs_str@1 = case input_ffi:input(
        <<"Enter the number of requests per node: "/utf8>>
    ) of
        {ok, Reqs_str} -> Reqs_str;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"main"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 26,
                        value => _assert_fail@1,
                        start => 651,
                        'end' => 738,
                        pattern_start => 662,
                        pattern_end => 674})
    end,
    Num_reqs = case gleam@int:base_parse(Reqs_str@1, 10) of
        {ok, Value@1} ->
            Value@1;

        {error, _} ->
            gleam_stdlib:println(
                <<"Invalid input. Using default: 10 requests."/utf8>>
            ),
            10
    end,
    gleam_stdlib:println(
        <<"Initializing Chord P2P Network Simulation..."/utf8>>
    ),
    Config_msg = <<<<<<<<"Network Size: "/utf8,
                    (erlang:integer_to_binary(Num_nodes))/binary>>/binary,
                " nodes, "/utf8>>/binary,
            (erlang:integer_to_binary(Num_reqs))/binary>>/binary,
        " requests each."/utf8>>,
    gleam_stdlib:println(Config_msg),
    Main_process_subject = gleam@erlang@process:new_subject(),
    Statistics_actor_builder = begin
        _pipe = gleam@otp@actor:new(
            chord_stats:init(Num_nodes, Num_reqs, Main_process_subject)
        ),
        gleam@otp@actor:on_message(_pipe, fun chord_stats:update/2)
    end,
    Stats_actor_pid = case gleam@otp@actor:start(Statistics_actor_builder) of
        {ok, Actor_ref} ->
            erlang:element(3, Actor_ref);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Fatal: Could not launch the statistics actor."/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"main"/utf8>>,
                    function => <<"main"/utf8>>,
                    line => 53})
    end,
    Supervisor_subject = chord_supervisor:start_supervisor(
        Num_nodes,
        Num_reqs,
        Stats_actor_pid
    ),
    gleam@otp@actor:send(Supervisor_subject, spawn_nodes),
    Very_long_timeout = 600000,
    case gleam@erlang@process:'receive'(Main_process_subject, Very_long_timeout) of
        {ok, {report, Average_hops}} ->
            gleam_stdlib:println(
                <<"All nodes have completed their lookups. Final average hops: "/utf8,
                    (gleam_stdlib:float_to_string(Average_hops))/binary>>
            ),
            gleam_stdlib:println(
                <<"Simulation has concluded successfully."/utf8>>
            );

        {error, _} ->
            gleam_stdlib:println(
                <<"Error: Simulation timed out before completion."/utf8>>
            );

        {ok, _} ->
            gleam_stdlib:println(
                <<"Received an intermediate message. Awaiting final result."/utf8>>
            )
    end.
