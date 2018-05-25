module Cotangent where
  import Tangent
  --Main function
  --Input => cotangent [LIST_OF_ELEMENTS_IN_RADIANS]
  cotangent::[Double]->[Double]
  cotangent list = map (_cotangent) list
  --Support function to calculate an individual cotangent
  --Input => _cotangent VALUE_TO_CALCULATE_IN_RADIANS
  _cotangent::Double->Double
  _cotangent var = (1/(_tangent var))
