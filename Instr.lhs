Instruction set definitions

> module Instr where
> import CLaSH.Prelude
> import Bitmanip
> import CVal


Instruction layouts:

	D-type instructions. These are pure operations on data on the `crb`. Both
	`arg 1` and `arg 2` are the operands to the operation if it is a binary
	operation. Only `arg 1` is used otherwise. `size` determines the bit-width
	of the arguments: a `size` of 0 means 8-bit wide arguments, 1 corresponds
	to 16-bit wide arguments and so on.

<	0   1                   6              10              14      16
<	+---+-------------------+---------------+---------------+-------+
<	| 1 |       opcode      |     arg 1     |     arg 2     |  size |
<	+---+-------------------+---------------+---------------+-------+


	M-type instructions. These load `CVal`s between the `crb` and registers and
	memory. `arg 1` is either the `CVal` that contains the memory address, or
	the register, depending on where it is loaded/stored from. `arg 2` is the
	`CVal` to be stored, if the instruction is a store instruction. `size` 
	determines how many bits are loaded or stored.

<	0           3           6              10              14      16
<	+-----------+-----------+---------------+---------------+-------+
<	|    000    |   mem-op  |     arg 1     |     arg 2     |  size |
<	+-----------+-----------+---------------+---------------+-------+


	C-type instructions. These are used to modify the state of the `crb` in
	order to be able to more easily execute loops and function calls.  `arg 1`
	may be a `CVal`.

<	0           3           6              10              14      16
<	+-----------+-----------+---------------+---------------+-------+
<	|    001    |    c-op   |     arg 1     |     arg 2     |  size |
<	+-----------+-----------+---------------+---------------+-------+


> type Size = BitVector 2
>
> data Instr
> 	= M MemOp  CPtr CPtr Size
> 	| D Opcode CPtr CPtr Size
> 	| C COp    CPtr CPtr Size
> 	deriving (Eq, Show)
>
> decode :: BitVector 16 -> Instr
> decode input
> 	| input .&. 0xe000 == 0x0000 = M miniop arg1 arg2 size
> 	| input .&. 0x8000 == 0x8000 = D opcode arg1 arg2 size
> 	| input .&. 0xe000 == 0x2000 = C miniop arg1 arg2 size
> 	where
> 		miniop = unpack $ bitSlice d3  d3 input
> 		opcode = unpack $ bitSlice d1  d5 input
> 		arg1   = unpack $ bitSlice d6  d4 input
> 		arg2   = unpack $ bitSlice d10 d4 input
> 		size   = unpack $ bitSlice d14 d2 input


All operations together with their opcodes are listed below. There are two
different ways of representing all operation types: using the bit pattern as
stored in memory or using a more type safe data type. The methods `pack` and
`unpack` are used to convert between the two. When they are synthesized, the
encodings are identical and the `pack` and `unpack` functions become the
identity function.

	'pure' operations and their opcodes (D-type).

> data Opcode
> 	= Add
> 	| Sub
> 	| Muli
> 	| Mulf
> 	deriving (Eq, Show, Read)

	Encodings for all Opcodes:

> instance BitPack Opcode where
> 	type BitSize Opcode = 5
>
> 	pack Add  = 0x00
> 	pack Sub  = 0x01
> 	pack Muli = 0x02
> 	pack Mulf = 0x03
>
> 	unpack 0x00 = Add
> 	unpack 0x01 = Sub
> 	unpack 0x02 = Muli
> 	unpack 0x03 = Mulf

  memory access instructions (M-type):

> data MemOp
> 	= Ldreg
> 	| Ldmem
> 	| Streg
> 	| Stmem
> 	deriving (Eq, Show, Read)

	Encodings for memory/registers access instructions:

> instance BitPack MemOp where
> 	type BitSize MemOp = 3
>
> 	pack Ldreg = 0x00
> 	pack Ldmem = 0x01
> 	pack Streg = 0x02
> 	pack Stmem = 0x03
>
> 	unpack 0x00 = Ldreg
> 	unpack 0x01 = Ldmem
> 	unpack 0x02 = Streg
> 	unpack 0x03 = Stmem

	`crb` modification instructions (C-type):

> data COp
> 	= Cnop
> 	| Cinc
> 	| Cadd
> 	| Cjmp
> 	deriving (Eq, Show, Read)

	Encodings for `crb` modification instructions:

> instance BitPack COp where
> 	type BitSize COp = 3
>
> 	pack Cnop  = 0x00
> 	pack Cinc  = 0x01
> 	pack Cadd  = 0x02
> 	pack Cjmp  = 0x03
>
> 	unpack 0x00 = Cnop
> 	unpack 0x01 = Cinc
> 	unpack 0x02 = Cadd
> 	unpack 0x03 = Cjmp
