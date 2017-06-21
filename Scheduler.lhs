Instruction Scheduler

> module Scheduler where
> import CLaSH.Prelude
> import Memory
> import Instr

`SState` holds all internal state for the instruction scheduler. This is
currently only the instruction pointer. It is not visible to external
components.

> data SState = SState
> 	{ instPtr :: BitVector 8
> 	}
> 	deriving Show


The Instruction Scheduler decodes and keeps track of which instructions should
be executed, loads them from instruction memory, and decodes them.

> scheduler :: Signal (BitVector 16) -> (Signal Instr, Signal IPtr)
> scheduler = unbundle . mealy scheduler' (SState 0)
>
> scheduler' :: SState -> BitVector 16 -> (SState, (Instr, IPtr))
> scheduler' state input = (state', (instr, instPtr state'))
> 	where
> 		state' = state { instPtr = instPtr state + 1 }
> 		instr = decode input
