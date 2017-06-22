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
> 	, buf :: Vec CRBSize CVal
> 	}
> 	deriving Show

For single-issue single clock cycle achitectures, the `crb` must be able to
read three values and write one value every clock cycle. `readA`, `readB`, and
`readC` are address offsets on the `crb` from the current write position. That
is, if `nextWriteA` has a value of 5, and `readA` has a value of 3, position 1
will be read from the `crb` and returned in `resA`. This means a read offset of
0 will read the result of the previous task, a read offset of 1 will read the
result of the task before that, etc. `nextWriteA` is an absolute address, and
should be used for the next task's `writeA` (which is also treated as an
absolute address).

> data CRead = CRead
> 	{ op :: COp
> 	, cReadA :: CPtr
> 	, cReadB :: CPtr
> 	, cReadC :: CPtr
> 	}
> 	deriving Show
> 	
> data CWrite = CWrite
> 	{ cWriteA :: CPtr
> 	, cWriteV :: CVal
> 	}
> 	deriving Show
>
> data COutput = COutput
> 	{ resA :: CVal
> 	, resB :: CVal
> 	, resC :: CVal
> 	, nextWriteA :: CPtr
> 	}
> 	deriving Show


Because of the lack of good blockRam primitives in CLaSH, the `crb` is
currently implemented as logic. Its type has been abstracted to be able to
support a later rewrite using proper blockRams.

> crb :: Signal CRead -> Signal CWrite -> Signal COutput
> crb = curry (mealy crb' startState . bundle)
> 	where
> 		startState = Crb 0x0 $ repeat $ CVal 5 0
>
> crb' :: Crb -> (CRead, CWrite) -> (Crb, COutput)
> crb' c (r, w) = (c' , COutput resA resB resC (regCounter c'))
> 	where
> 		c' = invalidateBuf r $ writeBuf w $ nextRC r c
> 		resA = buf c !! (regCounter c - cReadA r)
> 		resB = buf c !! (regCounter c - cReadB r)
> 		resC = buf c !! (regCounter c - cReadC r)
>
> writeBuf :: CWrite -> Crb -> Crb
> writeBuf i c = c { buf = replace (cWriteA i) (cWriteV i) (buf c) }

The `register counter` automatically increases every time a task is dispatched,
in order to allocate space for that task's result. This means an implicit `Cinc`
instruction is executed during these tasks. If a different C-type instruction
is executed, the `register counter` may change differently.

> nextRC :: CRead -> Crb -> Crb
> nextRC i c = c { regCounter = case op i of
> 		Cnop -> regCounter c
> 		Cinc -> regCounter c + 1
> 		Cadd -> regCounter c + cReadA i
> 		Cjmp -> cReadA i
> 	}

Every time a new task is dispatched, the contents of destination register for
that task have to be invalidated in order to prevent subsequent tasks from
reading the previously stored value instead of the one from the new task.

> invalidateBuf :: CRead -> Crb -> Crb
> invalidateBuf i c = c { buf = replace (regCounter c) wrt (buf c) }
> 	where
> 		wrt = flip ($) (buf c !! regCounter c) $ case op i of
> 			Cinc -> invalidate
> 			_    -> id
