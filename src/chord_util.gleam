import chord_math
import gleam/int

// -------------------
// Utility Functions
// -------------------

/// Hashes an integer to fit within the Chord identifier space, [0, 2^m).
pub fn map_to_id_space(value: Int, m_bits: Int) -> Int {
  let ring_size = chord_math.integer_power(2, m_bits)
  int.absolute_value(value) % ring_size
}

/// Calculates the starting identifier for a finger table entry.
/// The i-th finger of node n is responsible for the key at (n + 2^(i-1)).
pub fn compute_finger_start(node_id: Int, finger_index: Int, m_bits: Int) -> Int {
  let ring_size = chord_math.integer_power(2, m_bits)
  let power_of_2 = chord_math.integer_power(2, finger_index - 1)
  let start_id = { node_id + power_of_2 } % ring_size
  start_id
}

/// Generates a random key within the identifier space [0, 2^m).
pub fn generate_random_key(m_bits: Int) -> Int {
  let upper_bound = chord_math.integer_power(2, m_bits)
  int.random(upper_bound)
}

/// Checks if an ID is within the interval (start, end] on the Chord ring.
/// Handles the wrap-around case where start > end.
pub fn is_in_interval_inclusive(id: Int, start: Int, end_val: Int) -> Bool {
  case start < end_val {
    // If the interval does not wrap around the ring
    True -> id > start && id <= end_val
    // If the interval wraps around
    False -> id > start || id <= end_val
  }
}

/// Checks if an ID is within the exclusive interval (start, end) on the Chord ring.
/// Handles the wrap-around case.
pub fn is_in_interval_exclusive(id: Int, start: Int, end_val: Int) -> Bool {
  case start < end_val {
    // Standard interval case
    True -> id > start && id < end_val
    // Wrap-around interval case
    False -> id > start || id < end_val
  }
}
