Task Dispatcher

> module Dispatcher where
> import CLaSH.Prelude
> import RegisterBuffer
> import Instr
> import CVal

An `ATask` represents a computation that should be performed by an `alu`. It 
contains the source instruction for this task, all arguments from the `crb`, as
well as a pointer to the destination register for the result.

> data ATask = ATask
> 	{ instr :: Instr
> 	, aArgA :: CVal
> 	, aArgB :: CVal
> 	, aArgC :: CVal
> 	, resPtr :: CPtr
> 	}

A `CTask` represents some work that the `crb` needs to perform for the
`Dispatcher`.

> data CTask = CTask 
> 	{ cop :: COp 
> 	, cReadA :: CPtr 
> 	, cReadB :: CPtr 
> 	, cReadC :: CPtr
> 	}

`gather` is the first step of the `Dispatcher`. Here, the instruction is 
repackaged and sent to the `crb` in order to retreive the arguments for the 
computation.

> gather = liftA gather'
>
> gather' :: Instr -> CTask 
> gather' (M memop arg1 arg2 size) = CTask 
> 	{ cop = Cinc 
> 	, cReadA = arg1 
> 	, cReadB = arg2
> 	, cReadC = arg2
> 	}

`dispatch` is the second and final step of the `Dispatcher`. `dispatch` packs
the instruction together with the arguments from the `crb` and sends it to the
`alu`.

> dispatch = liftA2 dispatch'
>
> dispatch' :: Instr -> COutput -> ATask
> dispatch' i c = ATask i (resA c) (resB c) (resC c) (nextWriteA c)
