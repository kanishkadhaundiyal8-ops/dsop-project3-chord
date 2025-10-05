import chord_msgs.{
  type ActorAddress, type Msg, Announce, FetchPredecessor, FindKey,
  FindSuccessorFor, Heartbeat, RequestJoin, ReturnPredecessor, RunStabilization,
  SetSuccessor, StartLookups,
}
import chord_stats
import chord_util
import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/otp/actor

pub type FingerEntry {
  FingerEntry(start_id: Int, node_ref: ActorAddress)
}

pub type NodeState {
  NodeState(
    id: Int,
    m_bits: Int,
    self_ref: ActorAddress,
    stats_actor: Subject(chord_stats.StatsMessage),
    successor: ActorAddress,
    successor_id: Int,
    predecessor: Option(ActorAddress),
    predecessor_id: Option(Int),
    finger_table: List(FingerEntry),
    storage: dict.Dict(Int, String),
    pending_lookups: Int,
  )
}

pub fn init(
  id: Int,
  m: Int,
  stats: Subject(chord_stats.StatsMessage),
  self_ref: ActorAddress,
) -> NodeState {
  // A new node is its own successor initially.
  NodeState(id, m, self_ref, stats, self_ref, id, None, None, [], dict.new(), 0)
}

pub fn update(state: NodeState, msg: Msg) -> actor.Next(NodeState, Msg) {
  case msg {
    // A bootstrap node starts maintenance; others must join first.
    RequestJoin(bootstrap_node) if bootstrap_node == state.self_ref ->
      schedule_periodic_tasks(state)

    // A new node asks the bootstrap node to find its successor.
    RequestJoin(bootstrap_node) -> {
      actor.send(bootstrap_node, FindSuccessorFor(state.id, state.self_ref, 0))
      schedule_periodic_tasks(state)
    }

    // Find the node responsible for a key.
    FindSuccessorFor(key, requester, hops) -> {
      let pred_id = option.unwrap(state.predecessor_id, state.id)

      // If the key is between my predecessor and me, I am the successor.
      case chord_util.is_in_interval_inclusive(key, pred_id, state.id) {
        True -> {
          actor.send(
            requester,
            SetSuccessor(key, state.self_ref, state.id, hops + 1),
          )
          actor.continue(state)
        }
        False -> {
          // Otherwise, forward the request to the next closest node.
          let next_hop = find_closest_preceding_node(state, key)
          actor.send(next_hop, FindSuccessorFor(key, requester, hops + 1))
          actor.continue(state)
        }
      }
    }

    // A successor has been found for a key.
    SetSuccessor(key, successor_ref, new_successor_id, hop_count) -> {
      case key == state.id {
        // If this was for my own join request, adopt the successor.
        True -> {
          actor.send(successor_ref, Announce(state.self_ref, state.id))
          let new_state =
            NodeState(
              ..state,
              successor: successor_ref,
              successor_id: new_successor_id,
            )
          actor.continue(new_state)
        }
        // Otherwise, it was for a key lookup; report the stats.
        False -> {
          actor.send(state.stats_actor, chord_stats.AddHops(hop_count))
          actor.continue(state)
        }
      }
    }

    FindKey(key, _) -> {
      actor.send(state.self_ref, FindSuccessorFor(key, state.self_ref, 0))
      actor.continue(state)
    }

    StartLookups(count) -> {
      process.send_after(state.self_ref, 1000, Heartbeat)
      let new_state = NodeState(..state, pending_lookups: count)
      actor.continue(new_state)
    }

    // Periodically performs a random key lookup.
    Heartbeat -> {
      case state.pending_lookups {
        0 -> {
          io.println(
            "Node " <> int.to_string(state.id) <> " has finished all lookups.",
          )
          actor.send(state.stats_actor, chord_stats.NodeFinished)
          actor.continue(state)
        }
        n -> {
          let random_key = chord_util.generate_random_key(state.m_bits)
          actor.send(
            state.self_ref,
            FindSuccessorFor(random_key, state.self_ref, 0),
          )
          process.send_after(state.self_ref, 1000, Heartbeat)
          let new_state = NodeState(..state, pending_lookups: n - 1)
          actor.continue(new_state)
        }
      }
    }

    // --- Stabilization Protocol ---
    RunStabilization -> {
      actor.send(state.successor, FetchPredecessor(state.self_ref))
      schedule_periodic_tasks(state)
    }

    FetchPredecessor(requester) -> {
      actor.send(
        requester,
        ReturnPredecessor(state.predecessor, state.predecessor_id),
      )
      actor.continue(state)
    }

    ReturnPredecessor(pred_ref_opt, pred_id_opt) -> {
      let successor_has_new_pred = case pred_id_opt {
        Some(new_pred_id) ->
          chord_util.is_in_interval_exclusive(
            new_pred_id,
            state.id,
            state.successor_id,
          )
        None -> False
      }

      let updated_state = case successor_has_new_pred {
        True ->
          case pred_ref_opt, pred_id_opt {
            Some(new_pred_ref), Some(new_pred_id) ->
              NodeState(
                ..state,
                successor: new_pred_ref,
                successor_id: new_pred_id,
              )
            _, _ -> state
          }
        False -> state
      }
      actor.send(
        updated_state.successor,
        Announce(updated_state.self_ref, updated_state.id),
      )
      actor.continue(updated_state)
    }

    Announce(new_node_ref, new_node_id) -> {
      let should_adopt = case state.predecessor_id {
        None -> True
        Some(current_pred_id) ->
          chord_util.is_in_interval_exclusive(
            new_node_id,
            current_pred_id,
            state.id,
          )
      }

      let new_state = case should_adopt {
        True ->
          NodeState(
            ..state,
            predecessor: Some(new_node_ref),
            predecessor_id: Some(new_node_id),
          )
        False -> state
      }
      actor.continue(new_state)
    }

    // Stubs for future implementation
    _ -> actor.continue(state)
  }
}

fn schedule_periodic_tasks(state: NodeState) -> actor.Next(NodeState, Msg) {
  process.send_after(state.self_ref, 300, RunStabilization)
  actor.continue(state)
}

fn find_closest_preceding_node(state: NodeState, _key: Int) -> ActorAddress {
  state.successor
}

pub fn spawn_node_actor(
  id: Int,
  m: Int,
  stats: Subject(chord_stats.StatsMessage),
) -> ActorAddress {
  let builder =
    actor.new_with_initialiser(1000, fn(self_ref: ActorAddress) {
      let initial_state = init(id, m, stats, self_ref)
      actor.initialised(initial_state)
      |> actor.returning(self_ref)
      |> Ok
    })
    |> actor.on_message(update)

  case actor.start(builder) {
    Ok(actor_ref) -> actor_ref.data
    Error(_) -> panic as "Fatal: Failed to spawn a Chord node actor"
  }
}
