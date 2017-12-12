module Main

import Util
import BitVector
import Core
import Codecs

main : IO ()
main = printLn "hi"

{-  do let (MkCodec (MkEncoder e) (MkDecoder d)) = doubleCodec BigEndian 
     n <- getLine
     let res = e (cast {to=Double} n)
     printLn $ map showBin res 
     printLn $ res >>= d
-}