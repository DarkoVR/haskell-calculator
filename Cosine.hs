module Cosine where
  import Resources
  --Main function
  --Input => cosine [LIST_OF_ELEMENTS_IN_RADIANS]
  cosine::[Double]->[Double]
  cosine list = map (_cosine 20) list
  --Support function to calculate an individual cosine
  --Input => _cosine NUMBER_OF_REPETITIONS VALUE_TO_CALCULATE_IN_RADIANS
  _cosine::Integer->Double->Double
  _cosine 0 value = 1
  _cosine count value = (((power (-1) count)/(factorial (2*count)))*(power value (2*count)))+_cosine (count-1) value
