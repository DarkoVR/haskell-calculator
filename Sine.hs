module Sine where
  import Resources
  --Main function
  --Input => sine [LIST_OF_ELEMENTS_IN_RADIANS]
  sine::[Double]->[Double]
  sine list = map (_sine 20) list
  --Support function to calculate an individual sine
  --Input => _sine NUMBER_OF_REPETITIONS VALUE_TO_CALCULATE_IN_RADIANS
  _sine::Integer->Double->Double
  _sine 0 value = value
  _sine count value = (((power (-1) count)/(factorial (2*count+1)))*(power value (2*count+1)))+_sine (count-1) value
