module Roots where
  import Resources

  _evalFunc :: Double -> [(Double, Integer)] -> Double
  _evalFunc x f = sum (map ( \(n,m) -> n * (power x m ) ) f)

  _isRoot :: Double -> [(Double, Integer)] -> Bool
  _isRoot x f = _evalFunc x f >= -0.05e-2 && _evalFunc x f <= 0.05e-2

  raiz :: [(Double, Integer)] -> [Double]
  raiz f = filter (\x -> _isRoot x f) [0.0,0.1..100]