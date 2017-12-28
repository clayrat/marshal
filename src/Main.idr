module Main

import Data.Bits
import Data.Vect

import Util
import BitVector
import ByteVector
import Core
import Codecs

main : IO ()
main = do let ic = intCodec False BigEndian
          --let sc = stringCodec
          let mc = maybeCodec ic 
          --let ec = eitherTaggedCodec ic sc
          --let lc = listCodec mc
          let vnc = vectNCodec mc
          case encode vnc (_ ** [Just 17, Nothing, Just 23]) of 
            MkAttempt (Right res) => 
              do let bv = toByteVect res 
                 printLn bv
                 writeToFile "test.dat" bv
                 ls <- readFromFile "test.dat"
                 printLn ls
                 printLn $ decode vnc $ fromByteVect ls
            MkAttempt (Left err) => 
              do printLn err 
                 pure ()
          

