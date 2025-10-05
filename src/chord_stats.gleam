import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/int
import gleam/io
import gleam/otp/actor

pub type StatsMessage {
  AddHops(Int)
  NodeFinished
  Report(Float)
}

pub type StatisticsState {
  StatisticsState(
    hop_sum: Int,
    finished_node_count: Int,
    total_nodes: Int,
    requests_per_node: Int,
    main_process: Subject(StatsMessage),
  )
}

pub fn init(
  node_count: Int,
  req_count: Int,
  main_process: Subject(StatsMessage),
) -> StatisticsState {
  StatisticsState(0, 0, node_count, req_count, main_process)
}

pub fn update(
  state: StatisticsState,
  msg: StatsMessage,
) -> actor.Next(StatisticsState, StatsMessage) {
  case msg {
    AddHops(hop_count) ->
      actor.continue(
        StatisticsState(..state, hop_sum: state.hop_sum + hop_count),
      )

    NodeFinished -> {
      let new_finished_count = state.finished_node_count + 1
      let updated_state =
        StatisticsState(..state, finished_node_count: new_finished_count)

      let all_nodes_are_done = new_finished_count == state.total_nodes

      case all_nodes_are_done {
        True -> {
          let total_lookups = state.total_nodes * state.requests_per_node
          let average = case total_lookups {
            0 -> 0.0
            _ -> {
              let sum_float = int.to_float(state.hop_sum)
              let total_float = int.to_float(total_lookups)
              sum_float /. total_float
            }
          }
          // Send the final report back to the main process
          actor.send(state.main_process, Report(average))
        }
        False -> Nil
      }
      actor.continue(updated_state)
    }

    Report(_) -> actor.continue(state)
  }
}
