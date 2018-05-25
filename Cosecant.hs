module Cosecant where
  import Sine
  --Main function
  --Input => cosecat [LIST_OF_ELEMENTS_IN_RADIANS]
  cosecant::[Double]->[Double]
  cosecant list = map (_cosecant) list
  --Support function to calculate an individual cosecant
  --Input => _cosecant VALUE_TO_CALCULATE_IN_RADIANS
  _cosecant::Double->Double
  _cosecant var = (1/(_sine 20 var))
