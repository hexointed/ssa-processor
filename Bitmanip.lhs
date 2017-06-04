Bit manipulaiton functions

> module Bitmanip where
> import CLaSH.Prelude

Two types are bit-equal if they have the same number of bits and can be 
converted between each other freely

> type BitEq a b = (BitSize a ~ BitSize b, BitPack a, BitPack b)

> bitWith :: (BitEq a b, BitEq c a) => (b -> c) -> a -> a
> bitWith f = bitCoerce . f . bitCoerce


`bitTake n` extracts only the first `n` bits from a given `BitVector`

< 0         n         m
< +---...---+---...---+
< |  n bits |         |
< +---...---+---...---+
<  ^^^^^^^^^ 

> bitTake :: (KnownNat n, KnownNat m) => 
> 	SNat n -> BitVector (n + m) -> BitVector n
>
> bitTake n b = resize (shiftR b $ fromInteger $ natVal m)
> 	where m = bitDrop n b


`bitDrop n` extracts all but the first `n` bits from a given `BitVector`

< 0         n         m
< +---...---+---...---+
< |  n bits |         |
< +---...---+---...---+
<            ^^^^^^^^^

> bitDrop :: (KnownNat n, KnownNat m) => 
> 	SNat n -> BitVector (n + m) -> BitVector m
>
> bitDrop n b = resize b


`bitSlice n m` extracts `m` bits from a `BitVector`, starting at bit `n`

< 0         n       n+m         k
< +---...---+---...---+---...---+
< |  n bits |  m bits |         |
< +---...---+---...---+---...---+
<            ^^^^^^^^^

> bitSlice n m = bitDrop n . bitTake (addSNat m n)
