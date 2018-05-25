module Tangent where
  import Resources
  import Sine
  import Cosine
  --Main function
  --Input => tangent [LIST_OF_ELEMENTS_IN_RADIANS]
  tangent::[Double]->[Double]
  tangent list = map (_tangent) list
  --Support function to calculate an individual tangent
  --Input => _tangent VALUE_TO_CALCULATE_IN_RADIANS
  _tangent::Double->Double
  _tangent var = (_sine 20 var)/(_cosine 20 var)
