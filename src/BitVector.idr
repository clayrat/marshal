module BitVector

import Data.Primitives.Views

%default total
%access public export

data ByteOrdering = BigEndian | LittleEndian

BitVector : Type
BitVector = List Bool

showBin : BitVector -> String
showBin [] = ""
showBin (x :: xs) = (if x then "1" else "0") ++ showBin xs

isEmpty : BitVector -> Bool
isEmpty = isNil

empty : BitVector
empty = []

one : (n : Nat) -> BitVector 
one  Z    = []
one (S Z) = [True]
one (S n) = False :: one n 

zero : (n : Nat) -> BitVector 
zero n = replicate n False

size : BitVector -> Integer
size = cast . List.length

sizeGTE : BitVector -> Integer -> Bool
sizeGTE bv i = size bv >= i

head : (b : BitVector) -> {auto ok : NonEmpty b} -> Bool
head = List.head

tail : (b : BitVector) -> {auto ok : NonEmpty b} -> BitVector
tail = List.tail

take : (n : Integer) -> (b : BitVector) -> BitVector
take n = List.take (cast n)

drop : (n : Integer) -> (b : BitVector) -> BitVector
drop n = List.drop (cast n)

(++) : BitVector -> BitVector -> BitVector
(++) = List.(++)

parseInteger : (i : Integer) -> (bits : Integer) -> (ord : ByteOrdering) -> BitVector
parseInteger i bits ord = 
-- TODO ByteOrdering    
  if i >= 0 
    then go False i bits [] 
    else go True (-i-1) bits [] 
  where
    go : (neg : Bool) -> (i : Integer) -> (bits : Integer) -> (val : BitVector) -> BitVector
    go _   _ 0    val = val
    go neg i bits val with (divides i 2)
      go True ((2 * div) + mod) bits val | DivBy _ = assert_total $ go True div (bits-1) ((mod /= 1) :: val)
      go False ((2 * div) + mod) bits val | DivBy _ = assert_total $ go False div (bits-1) ((mod == 1) :: val)
    
toInteger : (sign : Bool) -> (ord : ByteOrdering) -> (b : BitVector) -> Integer
toInteger sign ord b = 
-- TODO ByteOrdering    
  case nonEmpty b of
    No _ => 0 
    Yes prf =>
      if sign 
        then 
          let lower = go (BitVector.tail {ok=prf} b) 0 in
          if head {ok=prf} b then -lower else lower
        else 
          go b 0      
  where
  go : (b : BitVector) -> (val : Integer) -> Integer     
  go [] val = val
  go (x :: xs) val = go xs (if x then 2*val+1 else 2*val)