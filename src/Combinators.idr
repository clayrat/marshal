module Combinators

import Util
import BitVector 
import Core

%default total
%access public export

-- BOOL

boolCodec : Codec Bool
boolCodec = 
  MkCodec 
   (MkEncoder $ \b => Right $ if b then one 1 else zero 1)
   (MkDecoder $ \bitv => 
     case nonEmpty bitv of
       No _ => Left $ insufficientBits 1 0 
       Yes prf => Right $ MkDecodeRes (head {ok=prf} bitv) (BitVector.tail {ok=prf} bitv))

-- INT

integerCodec : (bits : Int) -> (sign : Bool) -> (ord : ByteOrdering) -> Codec Integer
integerCodec bits sign ord =
  MkCodec
    (let bits' = cast {to=Integer} bits
         MaxValue = (shl 1 (if sign then bits' - 1 else bits')) - 1 
         MinValue = if sign then negate (shl 1 (bits' - 1)) else 0         
        in    
     MkEncoder $ \i => 
       if i > MaxValue 
         then Left $ show i ++ " is larger than maximum: " ++ show MaxValue
         else if i < MinValue 
            then Left $ show i ++ " is smaller than minimum: " ++ show MinValue
            else Right $ fromInteger i bits ord
    )
    (MkDecoder $ \bitv => 
      if sizeGTE bitv bits 
         then Right $ MkDecodeRes ((toInteger sign ord . take bits) bitv) (BitVector.drop bits bitv)
         else Left $ insufficientBits bits (size bitv))

intCodec : (bits : Int) -> (sign : Bool) -> (ord : ByteOrdering) -> Codec Int
intCodec bits sign ord =
  let (MkCodec e d) = integerCodec bits sign ord in 
  (MkCodec (contramap (cast {to=Integer}) e) (map fromInteger d))
