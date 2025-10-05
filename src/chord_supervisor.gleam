import chord_msgs.{type ActorAddress, RequestJoin, StartLookups}
import chord_node
import chord_stats
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/list
import gleam/otp/actor

pub type SupervisorMsg {
  SpawnNodes
  BeginSimulation
}

pub type SupervisorState {
  SupervisorState(
    node_count: Int,
    requests_per_node: Int,
    stats_actor: Subject(chord_stats.StatsMessage),
    node_refs: List(ActorAddress),
    id_space_bits: Int,
    supervisor_subject: Subject(SupervisorMsg),
  )
}

pub fn init(
  num_nodes: Int,
  num_reqs: Int,
  stats: Subject(chord_stats.StatsMessage),
) -> SupervisorState {
  let placeholder = fn() {
    panic as "Supervisor self-reference not initialized"
  }
  let m_bits = calculate_m_bits(num_nodes * 2)
  SupervisorState(num_nodes, num_reqs, stats, [], m_bits, placeholder())
}

pub fn update(
  state: SupervisorState,
  msg: SupervisorMsg,
) -> actor.Next(SupervisorState, SupervisorMsg) {
  case msg {
    SpawnNodes -> {
      io.println(
        "Supervisor: Spawning "
        <> int.to_string(state.node_count)
        <> " nodes and joining them sequentially...",
      )

      let bootstrap_node =
        chord_node.spawn_node_actor(0, state.id_space_bits, state.stats_actor)

      let other_node_ids = list.range(1, state.node_count)

      let other_nodes =
        list.map(other_node_ids, fn(id) {
          chord_node.spawn_node_actor(
            id,
            state.id_space_bits,
            state.stats_actor,
          )
        })

      // Join nodes sequentially with a 100ms delay between each join
      let _ =
        list.fold(other_nodes, 0, fn(index, node) {
          let delay = { index + 1 } * 100
          process.send_after(node, delay, RequestJoin(bootstrap_node))
          // Return the next index for the accumulator
          index + 1
        })

      let all_nodes = [bootstrap_node, ..other_nodes]

      // Calculate a generous settle time before starting lookups
      let settle_time = state.node_count * 100 + 4000
      process.send_after(state.supervisor_subject, settle_time, BeginSimulation)

      let new_state = SupervisorState(..state, node_refs: all_nodes)
      actor.continue(new_state)
    }

    BeginSimulation -> {
      io.println("Supervisor: Ring stabilized. Initiating lookups.")
      list.each(state.node_refs, fn(node) {
        actor.send(node, StartLookups(state.requests_per_node))
      })
      actor.continue(state)
    }
  }
}

pub fn start_supervisor(
  num_nodes: Int,
  num_reqs: Int,
  stats: Subject(chord_stats.StatsMessage),
) -> Subject(SupervisorMsg) {
  let builder =
    actor.new_with_initialiser(1000, fn(self_subject: Subject(SupervisorMsg)) {
      let m = calculate_m_bits(num_nodes * 2)
      let initial_state =
        SupervisorState(num_nodes, num_reqs, stats, [], m, self_subject)
      actor.initialised(initial_state)
      |> actor.returning(self_subject)
      |> Ok
    })
    |> actor.on_message(update)

  case actor.start(builder) {
    Ok(actor_ref) -> actor_ref.data
    Error(_) -> panic as "Fatal: Could not start supervisor actor"
  }
}

pub fn calculate_m_bits(n: Int) -> Int {
  do_calculate_m_bits(n, 1, 0)
}

fn do_calculate_m_bits(n: Int, power_of_2: Int, bits: Int) -> Int {
  case power_of_2 < n {
    True -> do_calculate_m_bits(n, power_of_2 * 2, bits + 1)
    False -> bits
  }
}
