module Core

import BitVector

%default total
%access public export

data DecodeRes : a -> Type where
  MkDecodeRes : (x : a) -> (rem : BitVector) -> DecodeRes a

Show a => Show (DecodeRes a) where
  show (MkDecodeRes x rem) = show x ++ ", " ++ show rem

Functor DecodeRes where 
  map f (MkDecodeRes x rem) = MkDecodeRes (f x) rem

mapRem : (f : BitVector -> BitVector) -> DecodeRes a -> DecodeRes a
mapRem fr (MkDecodeRes x rem) = MkDecodeRes x (fr rem)

MErr : Type
MErr = String

insufficientBits : Integer -> Integer -> MErr
insufficientBits n m = "expected " ++ show n ++ " bits, received " ++ show m

data Attempt : a -> Type where
  MkAttempt : Either MErr a -> Attempt a

Show a => Show (Attempt a) where
  show (MkAttempt (Left e)) = "Error : " ++ e
  show (MkAttempt (Right a)) = show a

Functor Attempt where
  map _ (MkAttempt (Left e)) = MkAttempt (Left e)
  map f (MkAttempt (Right a)) = MkAttempt (Right (f a))

Applicative Attempt where
  pure a = MkAttempt $ Right a
  (MkAttempt (Left e1)) <*> (MkAttempt (Left e2)) = MkAttempt (Left (e1 ++ ";" ++ e2))
  (MkAttempt (Left e1)) <*> (MkAttempt (Right _)) = MkAttempt (Left e1)  
  (MkAttempt (Right _)) <*> (MkAttempt (Left e2)) = MkAttempt (Left e2)  
  (MkAttempt (Right f)) <*> (MkAttempt (Right a)) = MkAttempt (Right (f a)) 

Monad Attempt where
  (MkAttempt (Left e))  >>= f = MkAttempt (Left e)
  (MkAttempt (Right a)) >>= f = f a

Semigroup a => Semigroup (Attempt a) where
  (<+>) (MkAttempt (Right a)) (MkAttempt (Right b)) = MkAttempt (Right (a <+> b))
  (<+>) (MkAttempt (Right _)) (MkAttempt (Left e2)) = MkAttempt (Left e2)
  (<+>) (MkAttempt (Left e1)) (MkAttempt (Right _)) = MkAttempt (Left e1)
  (<+>) (MkAttempt (Left e1)) (MkAttempt (Left e2)) = MkAttempt (Left (e1 ++ ";" ++ e2))

Monoid a => Monoid (Attempt a) where
  neutral = MkAttempt $ Right neutral

-- Encoder

data Encoder : a -> Type where
  MkEncoder : (encode : (x : a) -> Attempt BitVector) -> Encoder a

contramap : (f : b -> a) -> Encoder a -> Encoder b
contramap f (MkEncoder encode) = MkEncoder (encode . f)

runE : (Encoder a) -> a -> Attempt BitVector
runE (MkEncoder encode) a = encode a

-- Decoder

data Decoder : a -> Type where
  MkDecoder : (decode : (bits : BitVector) -> Attempt (DecodeRes a)) -> Decoder a

Functor Decoder where
  map f (MkDecoder decode) = MkDecoder (map (map f) . decode)

runD : (Decoder a) -> BitVector -> Attempt (DecodeRes a)
runD (MkDecoder decode) bv = decode bv

-- Codec 

data Codec : a -> Type where
  MkCodec : (encoder : Encoder a) -> (decoder : Decoder a) -> Codec a 

encode : Codec a -> a -> Attempt BitVector
encode (MkCodec (MkEncoder e) (MkDecoder _)) a = e a

decode : Codec a -> BitVector -> Attempt (DecodeRes a)
decode (MkCodec (MkEncoder _) (MkDecoder d)) bv = d bv
    
--Profunctor Codec where

dimap : (f : b -> a) -> (g : a -> b) -> Codec a -> Codec b
dimap f g (MkCodec e d) = MkCodec (contramap f e) (map g d)