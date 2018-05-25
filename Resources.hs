module Resources where
  --Resource function to calculate the power of a number by a number
  --Input => power BASE POWERBY_NUMBER
  power::Double->Integer->Double
  power base 0 = 1
  power base powerby = base * power base (powerby-1)
  --Resource function to calculate a factorial's number
  --Input => factorial NUMBER
  factorial::Integer->Double
  factorial 0 = 1
  factorial number = fromInteger number * factorial (number -1)
