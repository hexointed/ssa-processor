The `cyclic register buffer`  

> module RegisterBuffer where
> import CLaSH.Prelude
> import Instr
> import CVal


This is the first and most important set of registers for the SSA processor. 
Most instructions source at least one of their operands from the `crb`, and 
D-type instructions (those that do actual calculations) always source both of
their arguments from the `crb`.

The argument registers on the `crb` are adressed by the operations that use 
them, but the destination register is determined by the `register counter`. The
`register counter` normally increases every clock cycle, but can be altered by
several different instructions (C-type).

> data Crb = Crb
> 	{ regCounter :: CPtr
> 	, buf :: Vec 32 CVal
> 	}

For single-issue single clock cycle achitectures, the `crb` must be able to 
read two values and write one value every clock cycle.

> data CInput = CInput
> 	{ op :: COp
> 	, readA :: CPtr
> 	, readB :: CPtr
> 	, writeV :: CVal
> 	}

> crb :: Signal CInput -> Signal (CVal, CVal)
> crb = mealy crb' (Crb 0 (repeat 0))

Because of the lack of good blockRam primitives in CLaSH, the `crb` is
currently implemented as logic. Its type has been abstracted to be able to
support a later rewrite using proper blockRam's.

> crb' :: Crb -> CInput -> (Crb, (CVal, CVal))
> crb' c i = (Crb rc' buf', (valA, valB))
> 	where
> 		rc'  = nextRC (op i) (regCounter c) (readA i)
> 		buf' = replace (regCounter c) wrt' (buf c) 
> 		valA = buf c !! readA i
> 		valB = buf c !! readB i
>		wrt' = case op i of
>			Cinc -> writeV i
>			_    -> buf c !! regCounter c

The `register counter` automatically increases after every instruction that 
generates a result, in order to allocate space for said result. This means an
implicit `Cinc` instruction is executed during these instructions. If a 
different C-type instruction is executed, the `register counter` may change
differently.

> nextRC op rc arg = case op of
> 	Cnop -> rc
> 	Cinc -> rc + 1
> 	Cadd -> rc + arg
> 	Cjmp -> arg
