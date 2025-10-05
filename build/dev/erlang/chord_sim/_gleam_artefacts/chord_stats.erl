-module(chord_stats).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\chord_stats.gleam").
-export([init/3, update/2]).
-export_type([stats_message/0, statistics_state/0]).

-type stats_message() :: {add_hops, integer()} |
    node_finished |
    {report, float()}.

-type statistics_state() :: {statistics_state,
        integer(),
        integer(),
        integer(),
        integer(),
        gleam@erlang@process:subject(stats_message())}.

-file("src\\chord_stats.gleam", 23).
-spec init(integer(), integer(), gleam@erlang@process:subject(stats_message())) -> statistics_state().
init(Node_count, Req_count, Main_process) ->
    {statistics_state, 0, 0, Node_count, Req_count, Main_process}.

-file("src\\chord_stats.gleam", 31).
-spec update(statistics_state(), stats_message()) -> gleam@otp@actor:next(statistics_state(), stats_message()).
update(State, Msg) ->
    case Msg of
        {add_hops, Hop_count} ->
            gleam@otp@actor:continue(
                {statistics_state,
                    erlang:element(2, State) + Hop_count,
                    erlang:element(3, State),
                    erlang:element(4, State),
                    erlang:element(5, State),
                    erlang:element(6, State)}
            );

        node_finished ->
            New_finished_count = erlang:element(3, State) + 1,
            Updated_state = {statistics_state,
                erlang:element(2, State),
                New_finished_count,
                erlang:element(4, State),
                erlang:element(5, State),
                erlang:element(6, State)},
            All_nodes_are_done = New_finished_count =:= erlang:element(4, State),
            case All_nodes_are_done of
                true ->
                    Total_lookups = erlang:element(4, State) * erlang:element(
                        5,
                        State
                    ),
                    Average = case Total_lookups of
                        0 ->
                            +0.0;

                        _ ->
                            Sum_float = erlang:float(erlang:element(2, State)),
                            Total_float = erlang:float(Total_lookups),
                            case Total_float of
                                +0.0 -> +0.0;
                                -0.0 -> -0.0;
                                Gleam@denominator -> Sum_float / Gleam@denominator
                            end
                    end,
                    gleam@otp@actor:send(
                        erlang:element(6, State),
                        {report, Average}
                    );

                false ->
                    nil
            end,
            gleam@otp@actor:continue(Updated_state);

        {report, _} ->
            gleam@otp@actor:continue(State)
    end.
