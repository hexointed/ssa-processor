> module Alu where

> import CLaSH.Prelude
> import Instr
> import CVal
> import RegisterBuffer

An `AluTask` represents a computation that should be performed by an `alu`. It
contains the source instruction for this task, all arguments from the `crb`, as
well as a pointer to the destination register for the result.

> data AluTask = AluTask
> 	{ instr :: Instr
> 	, aArgA :: CVal
> 	, aArgB :: CVal
> 	, aArgC :: CVal
> 	, resPtr :: CPtr
> 	}
> 	deriving Show

The ALU computes the result for all 'pure' instructions, such as the arithmetic
instructions.

> alu = liftA alu'
>
> alu' :: AluTask -> CWrite
> alu' task = CWrite (resPtr task) res
> 	where
> 		a' = value $ aArgA task
> 		b' = value $ aArgB task
> 		c' = value $ aArgC task
> 		res = case op of
> 			Add  -> CVal (a' + b') defaultFlags
> 			Sub  -> CVal (a' - b') defaultFlags
> 			Muli -> CVal (a' * b') defaultFlags
> 			Mulf -> undefined
> 		Instr (D op) arg1 arg2 size = instr task
