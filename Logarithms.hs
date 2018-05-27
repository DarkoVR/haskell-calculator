module Logarithms where
  import Resources
  --Main function
  --Input => logNat [LIST_OF_ELEMENTS_IN_NUMBER_FORMAT]

  logNatural::[Double]->[Double]
  logNatural list = map (\n -> _logNatural n 20) list
  --Support function to calculate a single natural logarithm
  _logNatural::Double->Integer->Double
  _logNatural x n = 2 * serie x n

  serie :: Double -> Integer -> Double
  serie x 0 = (x-1)/(x+1)
  serie x n = 
    let nVal = fromInteger n
    in ( 1 / ( 2 * nVal + 1 ) ) * power ( ( x - 1 ) / ( x + 1 ) ) ( 2 * n + 1 ) + serie x ( n - 1)

  anyLog:: Double -> [Double] ->[Double]
  anyLog base list = map ( \n -> _anyLog base n 500 ) list

  _anyLog :: Double -> Double -> Integer -> Double
  _anyLog base x n = ( _logNatural x n ) / ( _logNatural base n )