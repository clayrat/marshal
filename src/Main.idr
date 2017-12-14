module Main

import Data.Bits
import Data.Vect

import Util
import BitVector
import Core
import Codecs

main : IO ()
main = do let ic = intCodec False BigEndian
          --let sc = stringCodec
          let mc = maybeCodec ic 
          --let ec = eitherTaggedCodec ic sc
          let lc = listCodec mc
          --let (MkCodec (MkEncoder e) (MkDecoder d)) = listCodec ic
          --let (MkCodec (MkEncoder e) (MkDecoder d)) = stringCodec
          --let (MkCodec (MkEncoder e) (MkDecoder d)) = vectCodec {n=3} ic
          --n <- getLine
          --let res = e (cast {to=Double} n)
          let res = encode lc [Just 17, Nothing, Just 23]
          printLn $ map showBin res
          printLn $ res >>= decode lc
