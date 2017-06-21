Per-core memory management.

> module Memory where
> import CLaSH.Prelude
> import Instr
> import CVal


Instruction memory. Non-mutable.

> type IPtr = BitVector 8
>
> instrMem :: Signal IPtr -> Signal (BitVector 16)
> instrMem addr = 
> 	mux (register True (pure False)) 0x8000 $
> 	romFile (SNat :: SNat 8) "instructions.dat" 
> 	(fmap (unpack . resize) addr)


Data memory, mutable. This module also keeps track of the `Thread registers`. 
The data memory is duplicated for each core for now, with no communication 
between cores. This should change soon, however.

> dataMem :: Signal MemOp -> Signal CVal -> Signal CVal -> Signal CVal
> dataMem mop addr val = fmap (\m -> CVal m 0) $ blockRam initial raddr' waddr'
> 	where
> 		initial  = repeat 0 :: Vec (2 ^ 8) (BitVector CValSize)
> 		raddr'   = fmap value addr
> 		waddr'   = fmap isRead mop <*> bundle (fmap value addr, fmap value val)
> 		isRead o = case o of
> 			Stmem -> Just
> 			_     -> \x -> Nothing
