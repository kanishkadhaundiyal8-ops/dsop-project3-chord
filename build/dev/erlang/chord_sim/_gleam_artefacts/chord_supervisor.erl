-module(chord_supervisor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\chord_supervisor.gleam").
-export([update/2, calculate_m_bits/1, init/3, start_supervisor/3]).
-export_type([supervisor_msg/0, supervisor_state/0]).

-type supervisor_msg() :: spawn_nodes | begin_simulation.

-type supervisor_state() :: {supervisor_state,
        integer(),
        integer(),
        gleam@erlang@process:subject(chord_stats:stats_message()),
        list(gleam@erlang@process:subject(chord_msgs:msg())),
        integer(),
        gleam@erlang@process:subject(supervisor_msg())}.

-file("src\\chord_supervisor.gleam", 38).
-spec update(supervisor_state(), supervisor_msg()) -> gleam@otp@actor:next(supervisor_state(), supervisor_msg()).
update(State, Msg) ->
    case Msg of
        spawn_nodes ->
            gleam_stdlib:println(
                <<<<"Supervisor: Spawning "/utf8,
                        (erlang:integer_to_binary(erlang:element(2, State)))/binary>>/binary,
                    " nodes and joining them sequentially..."/utf8>>
            ),
            Bootstrap_node = chord_node:spawn_node_actor(
                0,
                erlang:element(6, State),
                erlang:element(4, State)
            ),
            Other_node_ids = gleam@list:range(1, erlang:element(2, State)),
            Other_nodes = gleam@list:map(
                Other_node_ids,
                fun(Id) ->
                    chord_node:spawn_node_actor(
                        Id,
                        erlang:element(6, State),
                        erlang:element(4, State)
                    )
                end
            ),
            _ = gleam@list:fold(
                Other_nodes,
                0,
                fun(Index, Node) ->
                    Delay = (Index + 1) * 100,
                    gleam@erlang@process:send_after(
                        Node,
                        Delay,
                        {request_join, Bootstrap_node}
                    ),
                    Index + 1
                end
            ),
            All_nodes = [Bootstrap_node | Other_nodes],
            Settle_time = (erlang:element(2, State) * 100) + 4000,
            gleam@erlang@process:send_after(
                erlang:element(7, State),
                Settle_time,
                begin_simulation
            ),
            New_state = {supervisor_state,
                erlang:element(2, State),
                erlang:element(3, State),
                erlang:element(4, State),
                All_nodes,
                erlang:element(6, State),
                erlang:element(7, State)},
            gleam@otp@actor:continue(New_state);

        begin_simulation ->
            gleam_stdlib:println(
                <<"Supervisor: Ring stabilized. Initiating lookups."/utf8>>
            ),
            gleam@list:each(
                erlang:element(5, State),
                fun(Node@1) ->
                    gleam@otp@actor:send(
                        Node@1,
                        {start_lookups, erlang:element(3, State)}
                    )
                end
            ),
            gleam@otp@actor:continue(State)
    end.

-file("src\\chord_supervisor.gleam", 119).
-spec do_calculate_m_bits(integer(), integer(), integer()) -> integer().
do_calculate_m_bits(N, Power_of_2, Bits) ->
    case Power_of_2 < N of
        true ->
            do_calculate_m_bits(N, Power_of_2 * 2, Bits + 1);

        false ->
            Bits
    end.

-file("src\\chord_supervisor.gleam", 115).
-spec calculate_m_bits(integer()) -> integer().
calculate_m_bits(N) ->
    do_calculate_m_bits(N, 1, 0).

-file("src\\chord_supervisor.gleam", 26).
-spec init(
    integer(),
    integer(),
    gleam@erlang@process:subject(chord_stats:stats_message())
) -> supervisor_state().
init(Num_nodes, Num_reqs, Stats) ->
    Placeholder = fun() -> erlang:error(#{gleam_error => panic,
                message => <<"Supervisor self-reference not initialized"/utf8>>,
                file => <<?FILEPATH/utf8>>,
                module => <<"chord_supervisor"/utf8>>,
                function => <<"init"/utf8>>,
                line => 32}) end,
    M_bits = calculate_m_bits(Num_nodes * 2),
    {supervisor_state, Num_nodes, Num_reqs, Stats, [], M_bits, Placeholder()}.

-file("src\\chord_supervisor.gleam", 93).
-spec start_supervisor(
    integer(),
    integer(),
    gleam@erlang@process:subject(chord_stats:stats_message())
) -> gleam@erlang@process:subject(supervisor_msg()).
start_supervisor(Num_nodes, Num_reqs, Stats) ->
    Builder = begin
        _pipe@2 = gleam@otp@actor:new_with_initialiser(
            1000,
            fun(Self_subject) ->
                M = calculate_m_bits(Num_nodes * 2),
                Initial_state = {supervisor_state,
                    Num_nodes,
                    Num_reqs,
                    Stats,
                    [],
                    M,
                    Self_subject},
                _pipe = gleam@otp@actor:initialised(Initial_state),
                _pipe@1 = gleam@otp@actor:returning(_pipe, Self_subject),
                {ok, _pipe@1}
            end
        ),
        gleam@otp@actor:on_message(_pipe@2, fun update/2)
    end,
    case gleam@otp@actor:start(Builder) of
        {ok, Actor_ref} ->
            erlang:element(3, Actor_ref);

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"Fatal: Could not start supervisor actor"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"chord_supervisor"/utf8>>,
                    function => <<"start_supervisor"/utf8>>,
                    line => 111})
    end.
