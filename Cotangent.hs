module Cotangent where
  import Sine
  import Cosine
  --Main function
  --Input => tangent [LIST_OF_ELEMENTS_IN_RADIANS]
  cotangent::[Double]->[Double]
  cotangent list = map (_cotangent) list
  --Support function to calculate an individual tangent
  --Input => _tangent VALUE_TO_CALCULATE_IN_RADIANS
  _cotangent::Double->Double
  _cotangent var = (_cosine 20 var)/(_sine 20 var)
