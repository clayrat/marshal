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

insufficientBits : Int -> Int -> MErr
insufficientBits n m = "expected " ++ show n ++ " bits, received " ++ show m

data Encoder : a -> Type where
  MkEncoder : (encode : (x : a) -> Either MErr BitVector) -> Encoder a

contramap : (f : b -> a) -> Encoder a -> Encoder b
contramap f (MkEncoder encode) = MkEncoder (encode . f)

data Decoder : a -> Type where
  MkDecoder : (decode : (bits : BitVector) -> Either MErr (DecodeRes a)) -> Decoder a

Functor Decoder where
  map f (MkDecoder decode) = MkDecoder (map (map f) . decode)
  
data Codec : a -> Type where
  MkCodec : (encoder : Encoder a) -> (decoder : Decoder a) -> Codec a 
    
--Profunctor Codec where  

{-
interface Decoder a where
  decode : (bits : BitVector) -> Either MErr (DecodeRes a)

interface Encoder a where
  encode : (x : a) -> Either MErr BitVector

interface (Decoder a, Encoder a) => Codec a where
-}    