module Thag.Matics.Vulgar 
( binDigs
) where

import Data.Bits


{- 64-bit unsigned looks like the biggest numeric type that can be handled in
 - ... oh, yeah, in a 64-bit word.  hence the name.
 - for numbers > 2**64 -1, go fuck yourself! --}


--newtype Natural = Word


-- digits in order of increasing significance
-- (for n = 111010010, [ 0, 1, 0, 0, 1, 0, 1, 1, 1 n])
binDigs :: Word -> [Word]
binDigs n = loop n [] where
    loop 0 acc = acc
    loop n acc = (n .&. 1) : loop (n `shiftR` 1) acc

--loop m(n `shiftR` 1) (n .&. 1)







