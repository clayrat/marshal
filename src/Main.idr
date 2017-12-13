module Main

import Data.Bits
import Data.Vect

import Util
import BitVector
import Core
import Codecs

main : IO ()
main = do let ic = intCodec False BigEndian
          --let (MkCodec (MkEncoder e) (MkDecoder d)) = listCodec ic
          --let (MkCodec (MkEncoder e) (MkDecoder d)) = stringCodec
          let (MkCodec (MkEncoder e) (MkDecoder d)) = vectCodec {n=3} ic
          --n <- getLine
          --let res = e (cast {to=Double} n)
          let res = e (the (Vect 3 Int) [1,2,3])
          printLn $ map showBin res
          printLn $ res >>= d
