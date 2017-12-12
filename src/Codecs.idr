module Codecs

import Util
import BitVector 
import Core

%default total
%access public export

-- BOOL

boolCodec : Codec Bool
boolCodec = 
  MkCodec 
   (MkEncoder $ \b => MkAttempt $ Right $ if b then one 1 else zero 1)
   (MkDecoder $ \bitv => MkAttempt $
     case nonEmpty bitv of
       No _ => Left $ insufficientBits 1 0 
       Yes prf => Right $ MkDecodeRes (head {ok=prf} bitv) (BitVector.tail {ok=prf} bitv))

-- INT

integerCodec : (bits : Integer) -> (sign : Bool) -> (ord : ByteOrdering) -> Codec Integer
integerCodec bits sign ord =
  MkCodec
    (MkEncoder $ \i => MkAttempt $
      let max = (pow2 (if sign then bits-1 else bits)) - 1 
          min = if sign then negate (pow2 (bits-1)) else 0         
         in
      if i > max 
        then Left (show i ++ " is larger than maximum: " ++ show max)
        else if i < min
          then Left (show i ++ " is smaller than minimum: " ++ show min)
          else Right (parseInteger i bits ord)
    )
    (MkDecoder $ \bitv => MkAttempt $
      if sizeGTE bitv bits 
         then Right $ MkDecodeRes ((toInteger sign ord . BitVector.take bits) bitv) (BitVector.drop bits bitv)
         else Left $ insufficientBits bits (size bitv))

-- TODO Int platform size?         
intCodec : (bits : Integer) -> (sign : Bool) -> (ord : ByteOrdering) -> Codec Int
intCodec bits sign ord =
  let (MkCodec e d) = integerCodec bits sign ord in 
  (MkCodec (contramap (cast {to=Integer}) e) (map fromInteger d))

-- LIST

listCodec : (cdc : Codec a) -> Codec (List a)
listCodec cdc = 
  MkCodec
    (MkEncoder $ \la => 
      foldl (\att,a => att <+> encode cdc a) (MkAttempt $ Right $ BitVector.empty) la
    )
    (MkDecoder $ \bitv => go bitv [] 10000)  -- TODO a ^ param
  where 
  go : BitVector -> List a -> Int -> Attempt (DecodeRes (List a))
  go bv l cnt = 
    if isEmpty bv || cnt == 0
      then MkAttempt (Right (MkDecodeRes (List.reverse l) bv))
      else case decode cdc bv of 
        MkAttempt (Left err) => MkAttempt (Left err)
        MkAttempt (Right (MkDecodeRes x rem)) => go rem (x :: l) (assert_smaller cnt (cnt-1))

{-  
-- Double IEEE-754  

readDouble : Integer -> Double
readDouble i = go (parseInteger i sbits BigEndian) 0.5 0.0
  where
  go : BitVector -> Double -> Double -> Double
  go []        _  fn = fn
  go (x :: xs) bv fn = go xs (bv / 2) (if x then fn + bv else fn) 

doubleCodec : (ord : ByteOrdering) -> Codec Double
doubleCodec ord =
  let (MkCodec (MkEncoder e1) (MkDecoder d1)) = boolCodec 
      (MkCodec (MkEncoder e2) (MkDecoder d2)) = integerCodec ebits False BigEndian
      (MkCodec (MkEncoder e3) (MkDecoder d3)) = integerCodec sbits False BigEndian      
     in
  -- TODO ordering  
  MkCodec
    (MkEncoder $ \d => 
    let highExp = pow2 ebits - 1
        highSnf = pow2 sbits - 1
        (s, be, sf) = 
          if d == 0.0 
            then (False, 0, 0)
            else 
            case doubleVariety d of
              PInf => (False, highExp, 0)
              MInf => (True, highExp, 0)
              NaN  => (False, highExp, highSnf)  
              D v  => 
                let (sgn, vabs) = if v >= 0.0 then (False, v) else (True, -v) 
                    (exp, vnorm) = norm vabs
                    bexp = exp + bias
                    sgnf = cast {to=Integer} $ vnorm * (cast {to=Double} (pow2 sbits) + 0.5)
                   in
                (sgn, bexp, sgnf)
       in
      (e1 s) <+> (e2 be) <+> (e3 sf)
    )
   (MkDecoder $ \bitv => 
     --let at = the (Attempt (DecodeRes (Bool, Integer, Integer))) $ 
              do (MkDecodeRes s r1)   <- d1 bitv
                 (MkDecodeRes exp r2) <- d2 r1
                 (MkDecodeRes snf r3) <- d3 r2
                 let fn = readDouble snf
                 let sh = exp - bias
                 let d = ?ldexp 
                      -- if exp == 0 && snf == 0
                      -- then 0.0
                      -- else if sh == 128 && snf /= 0 
                      --  then sqrt (-1.0) 
                      --  else if sh == 128 && snf == 0 
                      --   then (if s then (-1.0/0/0) else (1.0/0.0))
                      --   else if sh > -127 
                      --    then ?ldexp
                  -- if s then -d else  
                 pure (MkDecodeRes d r3))
                           -}