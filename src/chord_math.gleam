/// Calculates integer power using tail recursion for safety.
pub fn integer_power(base: Int, exponent: Int) -> Int {
  power_recursive_helper(base, exponent, 1)
}

fn power_recursive_helper(base: Int, exponent: Int, accumulator: Int) -> Int {
  case exponent {
    0 -> accumulator
    _ -> power_recursive_helper(base, exponent - 1, base * accumulator)
  }
}
