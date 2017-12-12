module Main

import Data.Bits

import Util
import BitVector
import Core
import Codecs

main : IO ()
main = do --let ic = intCodec False BigEndian
          --let (MkCodec (MkEncoder e) (MkDecoder d)) = listCodec ic
          --let (MkCodec (MkEncoder e) (MkDecoder d)) = stringCodec
          let (MkCodec (MkEncoder e) (MkDecoder d)) = bitsCodec {n=8}
          --n <- getLine
          --let res = e (cast {to=Double} n)
          let res = e (intToBits 123)
          printLn $ map showBin res
          printLn $ res >>= d
