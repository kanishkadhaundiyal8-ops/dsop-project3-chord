-module(chord_node).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src\\chord_node.gleam").
-export([init/4, update/2, spawn_node_actor/3]).
-export_type([finger_entry/0, node_state/0]).

-type finger_entry() :: {finger_entry,
        integer(),
        gleam@erlang@process:subject(chord_msgs:msg())}.

-type node_state() :: {node_state,
        integer(),
        integer(),
        gleam@erlang@process:subject(chord_msgs:msg()),
        gleam@erlang@process:subject(chord_stats:stats_message()),
        gleam@erlang@process:subject(chord_msgs:msg()),
        integer(),
        gleam@option:option(gleam@erlang@process:subject(chord_msgs:msg())),
        gleam@option:option(integer()),
        list(finger_entry()),
        gleam@dict:dict(integer(), binary()),
        integer()}.

-file("src\\chord_node.gleam", 35).
-spec init(
    integer(),
    integer(),
    gleam@erlang@process:subject(chord_stats:stats_message()),
    gleam@erlang@process:subject(chord_msgs:msg())
) -> node_state().
init(Id, M, Stats, Self_ref) ->
    {node_state,
        Id,
        M,
        Self_ref,
        Stats,
        Self_ref,
        Id,
        none,
        none,
        [],
        maps:new(),
        0}.

-file("src\\chord_node.gleam", 208).
-spec schedule_periodic_tasks(node_state()) -> gleam@otp@actor:next(node_state(), chord_msgs:msg()).
schedule_periodic_tasks(State) ->
    gleam@erlang@process:send_after(
        erlang:element(4, State),
        300,
        run_stabilization
    ),
    gleam@otp@actor:continue(State).

-file("src\\chord_node.gleam", 213).
-spec find_closest_preceding_node(node_state(), integer()) -> gleam@erlang@process:subject(chord_msgs:msg()).
find_closest_preceding_node(State, _) ->
    erlang:element(6, State).

-file("src\\chord_node.gleam", 45).
-spec update(node_state(), chord_msgs:msg()) -> gleam@otp@actor:next(node_state(), chord_msgs:msg()).
update(State, Msg) ->
    case Msg of
        {request_join, Bootstrap_node} when Bootstrap_node =:= erlang:element(
            4,
            State
        ) ->
            schedule_periodic_tasks(State);

        {request_join, Bootstrap_node@1} ->
            gleam@otp@actor:send(
                Bootstrap_node@1,
                {find_successor_for,
                    erlang:element(2, State),
                    erlang:element(4, State),
                    0}
            ),
            schedule_periodic_tasks(State);

        {find_successor_for, Key, Requester, Hops} ->
            Pred_id = gleam@option:unwrap(
                erlang:element(9, State),
                erlang:element(2, State)
            ),
            case chord_util:is_in_interval_inclusive(
                Key,
                Pred_id,
                erlang:element(2, State)
            ) of
                true ->
                    gleam@otp@actor:send(
                        Requester,
                        {set_successor,
                            Key,
                            erlang:element(4, State),
                            erlang:element(2, State),
                            Hops + 1}
                    ),
                    gleam@otp@actor:continue(State);

                false ->
                    Next_hop = find_closest_preceding_node(State, Key),
                    gleam@otp@actor:send(
                        Next_hop,
                        {find_successor_for, Key, Requester, Hops + 1}
                    ),
                    gleam@otp@actor:continue(State)
            end;

        {set_successor, Key@1, Successor_ref, New_successor_id, Hop_count} ->
            case Key@1 =:= erlang:element(2, State) of
                true ->
                    gleam@otp@actor:send(
                        Successor_ref,
                        {announce,
                            erlang:element(4, State),
                            erlang:element(2, State)}
                    ),
                    New_state = {node_state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        erlang:element(4, State),
                        erlang:element(5, State),
                        Successor_ref,
                        New_successor_id,
                        erlang:element(8, State),
                        erlang:element(9, State),
                        erlang:element(10, State),
                        erlang:element(11, State),
                        erlang:element(12, State)},
                    gleam@otp@actor:continue(New_state);

                false ->
                    gleam@otp@actor:send(
                        erlang:element(5, State),
                        {add_hops, Hop_count}
                    ),
                    gleam@otp@actor:continue(State)
            end;

        {find_key, Key@2, _} ->
            gleam@otp@actor:send(
                erlang:element(4, State),
                {find_successor_for, Key@2, erlang:element(4, State), 0}
            ),
            gleam@otp@actor:continue(State);

        {start_lookups, Count} ->
            gleam@erlang@process:send_after(
                erlang:element(4, State),
                1000,
                heartbeat
            ),
            New_state@1 = {node_state,
                erlang:element(2, State),
                erlang:element(3, State),
                erlang:element(4, State),
                erlang:element(5, State),
                erlang:element(6, State),
                erlang:element(7, State),
                erlang:element(8, State),
                erlang:element(9, State),
                erlang:element(10, State),
                erlang:element(11, State),
                Count},
            gleam@otp@actor:continue(New_state@1);

        heartbeat ->
            case erlang:element(12, State) of
                0 ->
                    gleam_stdlib:println(
                        <<<<"Node "/utf8,
                                (erlang:integer_to_binary(
                                    erlang:element(2, State)
                                ))/binary>>/binary,
                            " has finished all lookups."/utf8>>
                    ),
                    gleam@otp@actor:send(
                        erlang:element(5, State),
                        node_finished
                    ),
                    gleam@otp@actor:continue(State);

                N ->
                    Random_key = chord_util:generate_random_key(
                        erlang:element(3, State)
                    ),
                    gleam@otp@actor:send(
                        erlang:element(4, State),
                        {find_successor_for,
                            Random_key,
                            erlang:element(4, State),
                            0}
                    ),
                    gleam@erlang@process:send_after(
                        erlang:element(4, State),
                        1000,
                        heartbeat
                    ),
                    New_state@2 = {node_state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        erlang:element(4, State),
                        erlang:element(5, State),
                        erlang:element(6, State),
                        erlang:element(7, State),
                        erlang:element(8, State),
                        erlang:element(9, State),
                        erlang:element(10, State),
                        erlang:element(11, State),
                        N - 1},
                    gleam@otp@actor:continue(New_state@2)
            end;

        run_stabilization ->
            gleam@otp@actor:send(
                erlang:element(6, State),
                {fetch_predecessor, erlang:element(4, State)}
            ),
            schedule_periodic_tasks(State);

        {fetch_predecessor, Requester@1} ->
            gleam@otp@actor:send(
                Requester@1,
                {return_predecessor,
                    erlang:element(8, State),
                    erlang:element(9, State)}
            ),
            gleam@otp@actor:continue(State);

        {return_predecessor, Pred_ref_opt, Pred_id_opt} ->
            Successor_has_new_pred = case Pred_id_opt of
                {some, New_pred_id} ->
                    chord_util:is_in_interval_exclusive(
                        New_pred_id,
                        erlang:element(2, State),
                        erlang:element(7, State)
                    );

                none ->
                    false
            end,
            Updated_state = case Successor_has_new_pred of
                true ->
                    case {Pred_ref_opt, Pred_id_opt} of
                        {{some, New_pred_ref}, {some, New_pred_id@1}} ->
                            {node_state,
                                erlang:element(2, State),
                                erlang:element(3, State),
                                erlang:element(4, State),
                                erlang:element(5, State),
                                New_pred_ref,
                                New_pred_id@1,
                                erlang:element(8, State),
                                erlang:element(9, State),
                                erlang:element(10, State),
                                erlang:element(11, State),
                                erlang:element(12, State)};

                        {_, _} ->
                            State
                    end;

                false ->
                    State
            end,
            gleam@otp@actor:send(
                erlang:element(6, Updated_state),
                {announce,
                    erlang:element(4, Updated_state),
                    erlang:element(2, Updated_state)}
            ),
            gleam@otp@actor:continue(Updated_state);

        {announce, New_node_ref, New_node_id} ->
            Should_adopt = case erlang:element(9, State) of
                none ->
                    true;

                {some, Current_pred_id} ->
                    chord_util:is_in_interval_exclusive(
                        New_node_id,
                        Current_pred_id,
                        erlang:element(2, State)
                    )
            end,
            New_state@3 = case Should_adopt of
                true ->
                    {node_state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        erlang:element(4, State),
                        erlang:element(5, State),
                        erlang:element(6, State),
                        erlang:element(7, State),
                        {some, New_node_ref},
                        {some, New_node_id},
                        erlang:element(10, State),
                        erlang:element(11, State),
                        erlang:element(12, State)};

                false ->
                    State
            end,
            gleam@otp@actor:continue(New_state@3);

        _ ->
            gleam@otp@actor:continue(State)
    end.

-file("src\\chord_node.gleam", 217).
-spec spawn_node_actor(
    integer(),
    integer(),
    gleam@erlang@process:subject(chord_stats:stats_message())
) -> gleam@erlang@process:subject(chord_msgs:msg()).
spawn_node_actor(Id, M, Stats) ->
    Builder = begin
        _pipe@2 = gleam@otp@actor:new_with_initialiser(
            1000,
            fun(Self_ref) ->
                Initial_state = init(Id, M, Stats, Self_ref),
                _pipe = gleam@otp@actor:initialised(Initial_state),
                _pipe@1 = gleam@otp@actor:returning(_pipe, Self_ref),
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
                    message => <<"Fatal: Failed to spawn a Chord node actor"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"chord_node"/utf8>>,
                    function => <<"spawn_node_actor"/utf8>>,
                    line => 233})
    end.
