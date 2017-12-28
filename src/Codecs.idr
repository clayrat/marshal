module Codecs

import Data.Vect
import Data.Bits

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

-- INT & BITS

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
intCodec : (sign : Bool) -> (ord : ByteOrdering) -> Codec Int
intCodec sign ord =
  let (MkCodec e d) = integerCodec 64 sign ord in
  (MkCodec (contramap (cast {to=Integer}) e) (map fromInteger d))

bitsCodec : Codec (Bits n)
bitsCodec {n} =
  MkCodec
  (MkEncoder $ \bn => MkAttempt $ Right (parseBits bn))
  (MkDecoder $ \bitv => MkAttempt $
    let n' = toIntegerNat n in
    if sizeGTE bitv n'
      then Right $ MkDecodeRes ((toBits n . BitVector.take n') bitv) (BitVector.drop n' bitv)
      else Left $ insufficientBits n' (size bitv))

-- NAT

natCodec : (bits : Integer) -> Codec Nat
natCodec bits =
  let (MkCodec e d) = integerCodec bits False BigEndian in
  (MkCodec (contramap (cast {to=Integer}) e) (map (fromInteger) d))

-- LIST
-- TODO length-encoded version? mark greedy codecs?
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

-- CHAR

charCodec : Codec Char
charCodec =
  let (MkCodec e d) = integerCodec 16 False BigEndian in -- TODO platform dependent?
  (MkCodec (contramap (cast {to=Integer} . ord) e) (map (chr . fromInteger) d))

-- STRING
-- TODO length-encoded version? mark greedy codecs?
stringCodec : Codec String
stringCodec =
  let (MkCodec e d) = listCodec charCodec in
  (MkCodec (contramap unpack e) (map pack d))

-- VECT

vectCodec : Codec a -> Codec (Vect n a)
vectCodec {n} cdc =
  MkCodec
    (MkEncoder $ \va =>
      foldl (\att,a => att <+> encode cdc a) (pure BitVector.empty) va
    )
    (MkDecoder $ \bitv => go n bitv Z [])  -- TODO a ^ param
  where
  go : (d : Nat) -> BitVector -> (k : Nat) -> Vect k a -> Attempt (DecodeRes (Vect n a))  -- recurse on nat to convince termination checker
  go Z bv k v = 
    MkAttempt $
    case decEq k n of 
      Yes prf => rewrite sym prf in 
                 Right (MkDecodeRes v bv)
      No _ => Left ("Expected " ++ show n ++ " elements")
  go (S d) bv k v = 
    if isEmpty bv 
      then go d bv k v 
       else 
        case decode cdc bv of
          MkAttempt (Left err) => MkAttempt (Left err)
          MkAttempt (Right (MkDecodeRes x rem)) => 
            go d rem (S k) (rewrite plusCommutative 1 k in v ++ [x])

vectNCodec : Codec a -> Codec (DPair Nat (\n => Vect n a))
vectNCodec {a} cdc = 
  let sizeCdc = natCodec 8 in -- TODO configure? 
  MkCodec
    (MkEncoder $ \(n**v) => encode sizeCdc n <+> foldl (\att,a => att <+> encode cdc a) (pure BitVector.empty) v)
    (MkDecoder $ \bitv => 
      do (MkDecodeRes n r1) <- decode sizeCdc bitv
         (MkDecodeRes v r2) <- go n r1 []
         pure (MkDecodeRes (n ** v) r2)
    )
  where 
  go : (n : Nat) -> BitVector -> Vect m a -> Attempt (DecodeRes (Vect (m+n) a))  
  go {m} Z    rem acc = pure (MkDecodeRes (rewrite plusCommutative m 0 in acc) rem)
  go {m} (S k) rem acc = 
    case decode cdc rem of
      MkAttempt (Left err) => MkAttempt (Left err)
      MkAttempt (Right (MkDecodeRes x rem)) => 
        rewrite plusAssociative m 1 k in 
        go k rem (acc ++ [x])

-- write sizes of both? mark greedy?
pairCodec : (cdca : Codec a) -> (cdcb : Codec b) -> Codec (Pair a b)            
pairCodec cdca cdcb =
  MkCodec 
    (MkEncoder $ \ab =>
       encode cdca (fst ab) <+> encode cdcb (snd ab))
    (MkDecoder $ \bitv =>
      do (MkDecodeRes a r1) <- decode cdca bitv
         (MkDecodeRes b r2) <- decode cdcb r1
         pure (MkDecodeRes (a,b) r2))

maybeCodec : (cdca : Codec a) -> Codec (Maybe a)
maybeCodec cdca@(MkCodec _ da) =
  let cdct = boolCodec in
  MkCodec 
    (MkEncoder $ \ma => 
       case ma of 
         Nothing => encode cdct False
         Just a => encode cdct True <+> encode cdca a)
    (MkDecoder $ \bitv =>
       do (MkDecodeRes t r) <- decode cdct bitv
          if t then runD (map Just da) r else pure (MkDecodeRes Nothing r))

eitherTaggedCodec : (cdca : Codec a) -> (cdcb : Codec b) -> Codec (Either a b)          
eitherTaggedCodec cdca@(MkCodec _ da) cdcb@(MkCodec _ db) =
  let cdct = boolCodec in
  MkCodec 
    (MkEncoder $ \ab => 
       case ab of 
         Left a => encode cdct False <+> encode cdca a
         Right b => encode cdct True <+> encode cdcb b)
    (MkDecoder $ \bitv =>
      do (MkDecodeRes t r1) <- decode cdct bitv
         (MkDecodeRes ab r2) <- runD (if t then map Right db else map Left da) r1 
         pure (MkDecodeRes ab r2))

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