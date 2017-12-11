module Main

import BitVector
import Core
import Combinators

main : IO ()
main = 
  do let (MkCodec (MkEncoder e) (MkDecoder d)) = intCodec 5 False BigEndian 
     let enc = e 31
     case enc of 
       Left err => printLn err
       Right res => 
         do printLn res
            case d res of 
              Left err => printLn err
              Right res2 => printLn res2