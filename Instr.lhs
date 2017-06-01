> {-# LANGUAGE GeneralizedNewtypeDeriving #-}

Instruction set definitions

> module Instr where
> import CLaSH.Prelude

All arithmetic Instructions source their operands from the `cyclic register 
buffer`. Each element on the `crb` is called a `CVal`, and a reference to a 
`CVal` is called a `CPtr`. Currently, the `crb` contains 32 values, and each
`CVal` is 32 bits wide.

> type CVal = BitVector 32
> type CPtr = BitVector 5


Instruction layouts:

	D-type instructions. These are pure operations on data on the `crb`.

	0    1                    6                   11                   16
	+----+--------------------+--------------------+--------------------+
	| 0  |       opcode       |       cptr 1       |       cptr 2       |
	+----+--------------------+--------------------+--------------------+


	M-type instructions. These load `CVal`s between the `crb` and registers and
	memory. `mptr` is either the `CVal` that contains the memory address, or
	the register, depending on where is loaded/stored from.

	0    1                    6                   11                   16
	+----+--------------------+--------------------+--------------------+
	| 1  |       opcode       |        mptr        |        cptr        |
	+----+--------------------+--------------------+--------------------+


> newtype Opcode = Opcode (BitVector 5)
> 	deriving (Bits, Enum, Eq, Integral, Num, Ord, Real, Show)
>
> instance BitPack Opcode where
> 	type BitSize (Opcode) = 5
> 	unpack = Opcode
> 	pack (Opcode o) = o


All operations together with their opcodes are listed below:

	'pure' operations and their opcodes (D-type):
	
> 	add  = 0x00 :: Opcode
> 	sub  = 0x01 :: Opcode
> 	muli = 0x02 :: Opcode
> 	mulf = 0x03 :: Opcode

	memory access instructions (M-type):

> 	streg = 0x00 :: Opcode
> 	ldreg = 0x01 :: Opcode
> 	stmem = 0x02 :: Opcode
> 	ldmem = 0x03 :: Opcode
