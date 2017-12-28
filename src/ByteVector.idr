module ByteVector

import Data.Primitives.Views
import Data.Bits
import Data.Buffer

import BitVector
import Util

%default total
%access public export

splitUnder : (n : Integer) -> (b : BitVector) -> Either BitVector (BitVector, BitVector)
splitUnder n b = 
  if sizeGTE b n then Right (take n b, drop n b) else Left b
    
toByteVect : (b : BitVector) -> List (Bits 8)
toByteVect [] = [] 
toByteVect b = go b [] 
  where 
  go : BitVector -> List (Bits 8) -> List (Bits 8)
  go b acc = case splitUnder 8 b of
    Left under => acc ++ [toBits 8 under] 
    Right (hd, rem) => assert_total $ go rem (acc ++ [toBits 8 hd])

fromByteVect : List (Bits 8) -> BitVector
fromByteVect [] = empty
fromByteVect (b::bs) = BitVector.(++) (parseBits b) (fromByteVect bs)

toBuffer : List (Bits 8) -> IO (Maybe Buffer)
toBuffer bs = 
  do let n = size bs
     mbuf <- newBuffer (cast n)
     maybe (pure ()) (\buf => go bs 0 buf) mbuf
     pure mbuf
  where 
  go : List (Bits 8) -> Int -> Buffer -> IO ()
  go []        _ _ = pure ()
  go (MkBits b :: bs) i buf = do setByte buf i b
                                 go bs (i+1) buf

fromBuffer : Buffer -> IO (List (Bits 8))
fromBuffer buf = do dat <- bufferData buf
                    pure (map MkBits dat)

writeToFile : (fname : String) -> List (Bits 8) -> IO ()
writeToFile fname bs = do Right f <- openFile fname WriteTruncate | Left err => pure ()
                          mbuf <- toBuffer bs
                          maybe (pure ()) (\buf => do writeBufferToFile f buf 65536
                                                      pure ()) mbuf
                          closeFile f

readFromFile : (fname : String) -> IO (List (Bits 8))
readFromFile fname = do Right f <- openFile fname Read | Left err => pure []
                        Right max <- fileSize f | Left err => pure []
                        mbuf <- newBuffer max
                        lin <- maybe (pure []) (\buf => readBufferFromFile f buf max >>= fromBuffer) mbuf 
                        closeFile f
                        pure lin