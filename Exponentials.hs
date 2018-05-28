module Exponentials where
  import Resources
  import Logarithms

  polinomial::Integer->[Double]->[Double]
  polinomial  powerby list = map ( \x -> power x powerby ) list

  exponential::Double->[Double]->[Double]
  exponential a list = map ( \x -> _exponential a x 100) list

  _exponential::Double->Double->Integer->Double
  _exponential a x 0 = 1
  _exponential a x n = 
    ( power x n ) * ( power (_logNatural a 100) n ) / (factorial n)  +
     _exponential a x ( n - 1 )