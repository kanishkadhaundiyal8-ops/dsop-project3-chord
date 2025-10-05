import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}

/// An alias for an actor's address, used to send messages to a node.
pub type ActorAddress =
  Subject(Msg)

/// Defines all messages that Chord node actors can send and receive.
pub type Msg {
  // Ring management
  RequestJoin(bootstrap_node: ActorAddress)
  SetSuccessor(
    key: Int,
    successor_ref: ActorAddress,
    successor_id: Int,
    hop_count: Int,
  )
  FindSuccessorFor(key: Int, reply_to: ActorAddress, hop_count: Int)

  // Stabilization protocol
  RunStabilization
  FetchPredecessor(reply_to: ActorAddress)
  ReturnPredecessor(predecessor: Option(ActorAddress), pred_id: Option(Int))
  Announce(new_node: ActorAddress, new_node_id: Int)

  // Finger table maintenance
  UpdateFingers(index: Int)
  VerifyPredecessor

  // Application-level
  StartLookups(count: Int)
  FindKey(key: Int, request_id: Int)
  Heartbeat
}
