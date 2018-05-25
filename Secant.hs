module Secant where
  import Cosine
  --Main function
  --Input => secant [LIST_OF_ELEMENTS_IN_RADIANS]
  secant::[Double]->[Double]
  secant list = map (_secant) list
  --Support function to calculate an individual secant
  --Input => _secant VALUE_TO_CALCULATE_IN_RADIANS
  _secant::Double->Double
  _secant var = (1/(_cosine 20 var))
