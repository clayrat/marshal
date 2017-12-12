module Main

import Util
import BitVector
import Core
import Codecs

main : IO ()
main = do let ic = intCodec 64 False BigEndian 
          let (MkCodec (MkEncoder e) (MkDecoder d)) = listCodec ic
          --n <- getLine
          --let res = e (cast {to=Double} n)
          let res = e [1,2,3]
          printLn $ map showBin res 
          printLn $ res >>= d
