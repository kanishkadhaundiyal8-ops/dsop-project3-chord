import gleam/erlang/process
import gleam/float
import gleam/int
import gleam/io
import gleam/otp/actor
import gleam/result
import input.{input}

// As seen in your example 

import chord_stats
import chord_supervisor

pub fn main() {
  // Get input for the number of nodes using an interactive prompt[cite: 45].
  let assert Ok(nodes_str) = input(prompt: "Enter the number of nodes: ")
  let num_nodes = case int.base_parse(nodes_str, 10) {
    Ok(value) -> value
    Error(_) -> {
      io.println("Invalid input. Using default: 50 nodes.")
      50
    }
  }

  // Get input for the number of requests per node[cite: 45].
  let assert Ok(reqs_str) =
    input(prompt: "Enter the number of requests per node: ")
  let num_reqs = case int.base_parse(reqs_str, 10) {
    Ok(value) -> value
    Error(_) -> {
      io.println("Invalid input. Using default: 10 requests.")
      10
    }
  }

  io.println("Initializing Chord P2P Network Simulation...")
  let config_msg =
    "Network Size: "
    <> int.to_string(num_nodes)
    <> " nodes, "
    <> int.to_string(num_reqs)
    <> " requests each."
  io.println(config_msg)

  let main_process_subject = process.new_subject()

  let statistics_actor_builder =
    actor.new(chord_stats.init(num_nodes, num_reqs, main_process_subject))
    |> actor.on_message(chord_stats.update)

  let stats_actor_pid = case actor.start(statistics_actor_builder) {
    Ok(actor_ref) -> actor_ref.data
    Error(_) -> panic as "Fatal: Could not launch the statistics actor."
  }

  let supervisor_subject =
    chord_supervisor.start_supervisor(num_nodes, num_reqs, stats_actor_pid)

  // Start the simulation
  actor.send(supervisor_subject, chord_supervisor.SpawnNodes)

  // Wait for the simulation to complete with a long timeout[cite: 49].
  let very_long_timeout = 600_000
  // 10 minutes
  case process.receive(main_process_subject, very_long_timeout) {
    Ok(chord_stats.Report(average_hops)) -> {
      io.println(
        "All nodes have completed their lookups. Final average hops: "
        <> float.to_string(average_hops),
      )
      io.println("Simulation has concluded successfully.")
    }
    Error(_) -> io.println("Error: Simulation timed out before completion.")
    Ok(_) ->
      io.println("Received an intermediate message. Awaiting final result.")
  }
}
