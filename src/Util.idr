module Util

import Data.Primitives.Views

%default total
%access public export

-- Integer

pow2 : Integer -> Integer 
pow2 n with (integerRec n)
  pow2 0 | IntegerZ = 1
  pow2 n | IntegerSucc v = 2 * pow2 (n-1) | v
  pow2 _ | IntegerPred _ = 0

-- Double

norm : Double -> (Integer, Double)
norm d = 
  let (e1, d1) = go1 0 d 
      (e2, d2) = go2 e1 d1
     in
  if e2 < -1022 then go3 e2 d2 else (e2, d2 - 1.0)
  where
  go1 : Integer -> Double -> (Integer, Double)
  go1 e d = if d < 2.0 then (e, d) else go1 (e+1) (assert_smaller d (d/2.0))
  go2 : Integer -> Double -> (Integer, Double)
  go2 e d = if d >= 1.0 then (e, d) else go2 (e-1) (assert_smaller d (d*2.0))
  go3 : Integer -> Double -> (Integer, Double)
  go3 e d = if e >= -1022 then (-1023, d) else go3 (assert_smaller e (e+1)) (d/2.0)

ebits : Integer
ebits = 11

sbits : Integer 
sbits = 52

bias : Integer
bias = (pow2 (ebits - 1)) - 1

data DoubleVariety = NaN | PInf | MInf | D Double

isNaN : Double -> Bool
isNaN d = d /= d

isPInf : Double -> Bool
isPInf d = (d >= 0.0) && (isNaN (d-d)) && not (isNaN d)

isMInf : Double -> Bool
isMInf d = (d < 0.0) && (isNaN (d-d)) && not (isNaN d)

doubleVariety : Double -> DoubleVariety
doubleVariety d = 
  if isNaN d then NaN
    else
      if isNaN (d-d) 
        then
          if d >= 0.0 then PInf else MInf   
        else D d


