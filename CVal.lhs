Definitions for `CVal`, the elemens on the `cyclic register buffer`

> module CVal where
> import CLaSH.Prelude
> import Bitmanip


All arithmetic Instructions source their operands from the `cyclic register 
buffer`. Each element on the `crb` is called a `CVal`, and a reference to a 
`CVal` is called a `CPtr`. Currently, the `crb` contains 16 values, and each
`CVal` is 32 bits wide.

> type CRBSize = 16
> type CValSize = 32
> 
> type CPtr = BitVector (Log 2 CRBSize)
> data CVal = CVal

	The main contents of a `CVal` is the `value`, which is the only data that
	can be directly manipulated by most instructions.

> 	{ value :: BitVector CValSize


	Each `CVal` also contains a set of `flags` that hold information about the
	state of the computation itself, rather than its result. These are used
	internally, and can also be used by e.g. jump instructions.

<	+---+---+---+---+
<	| C | V | X | R |
<	+---+---+---+---+

> 	, flags :: BitVector 4
> 	}
> 	deriving Show

> flagC :: CVal -> Bool
> flagC = bitCoerce . bitSlice d0 d1 . flags
>
> flagV :: CVal -> Bool
> flagV = bitCoerce . bitSlice d1 d1 . flags
>
> flagX :: CVal -> Bool
> flagX = bitCoerce . bitSlice d2 d1 . flags

The `R` flag is special, because it is used by the SSA processor to determine
whether an instruction has been completed or not. The `R` flag is automatically
zeroed before a task is dispatched to populate a `CVal`, and tasks with 
arguments whose `R` flags are zero must stall until they are one.

> ready :: CVal -> Bool
> ready = bitCoerce . bitSlice d3 d1 . flags
>
> invalidate :: CVal -> CVal
> invalidate c = c { flags = flags c .&. 0xe }
>
> fromValue :: BitVector CValSize -> CVal
> fromValue v = CVal v 0x1
>
> defaultFlags = low ++# low ++# low ++# high
