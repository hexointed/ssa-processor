> module Alu where

> import CLaSH.Prelude
> import Instr
> import CVal

The ALU computes the result for all 'pure' instructions, such as the arithmetic
instructions.

> alu :: Size -> Opcode -> CVal -> CVal -> CVal -> CVal
> alu size op a b c = case op of
> 	Add  -> CVal (a' + b') defaultFlags
> 	Sub  -> CVal (a' - b') defaultFlags
> 	Muli -> CVal (a' * b') defaultFlags
> 	Mulf -> undefined
> 	where
> 		a' = value a
> 		b' = value b
> 		c' = value c
