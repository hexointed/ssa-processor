Task Dispatcher

> module Dispatcher where
> import CLaSH.Prelude
> import RegisterBuffer
> import Instr
> import CVal
> import Alu

`fetchArgs` is the first step of the `Dispatcher`. Here, the instruction is
repackaged and sent to the `crb` in order to retreive the arguments for the
computation.

> fetchArgs = liftA fetchArgs'
>
> fetchArgs' :: Instr -> CRead
> fetchArgs' (Instr op arg1 arg2 size) = CRead cop arg1 arg2 arg2
> 	where
> 		cop = case op of
> 			D opcode -> Cinc
> 			M miniop -> Cinc
> 			C miniop -> miniop

`dispatch` is the second and final step of the `Dispatcher`. `dispatch` packs
the instruction together with the arguments from the `crb` and sends it to the
`alu`.

> dispatch = liftA2 dispatch'
>
> dispatch' :: Instr -> COutput -> AluTask
> dispatch' i c = AluTask i (resA c) (resB c) (resC c) (nextWriteA c)
