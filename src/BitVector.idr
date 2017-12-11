module BitVector

import Data.Primitives.Views

%default total
%access public export

data ByteOrdering = BigEndian | LittleEndian

BitVector : Type
BitVector = List Bool

isEmpty : BitVector -> Bool
isEmpty = isNil

one : (n : Nat) -> BitVector 
one  Z    = []
one (S Z) = [True]
one (S n) = False :: one n 

zero : (n : Nat) -> BitVector 
zero n = replicate n False

size : BitVector -> Int
size = cast . List.length

sizeGTE : BitVector -> Int -> Bool
sizeGTE bv i = size bv >= i

head : (b : BitVector) -> {auto ok : NonEmpty b} -> Bool
head = List.head

tail : (b : BitVector) -> {auto ok : NonEmpty b} -> BitVector
tail = List.tail

take : (n : Int) -> (b : BitVector) -> BitVector
take n = List.take (cast n)

drop : (n : Int) -> (b : BitVector) -> BitVector
drop n = List.drop (cast n)

fromInteger : (i : Integer) -> (bits : Int) -> (ord : ByteOrdering) -> BitVector
fromInteger i bits ord = 
-- TODO ByteOrdering    
  go i bits [] 
  where
    go : (i : Integer) -> (bits : Int) -> (val : BitVector) -> BitVector
    go _ 0    val = val
    go i bits val with (divides i 2)
      go ((2 * div) + mod) bits val | DivBy _ = go div (assert_smaller bits (bits-1)) ((mod == 1) :: val)
    
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