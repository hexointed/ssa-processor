A single processor core

> module Core where
> import CLaSH.Prelude
> import Instr
> import Memory
> import RegisterBuffer
> import Scheduler
> import Alu
> import Dispatcher


< +-------------------------------------------------------------+ 
< | Core                                                        |
< | +---------------------------+      +-----------------------------+
< | | Task Dispatcher           |   .->| Cache / Instruction memory  |
< | +---------------------------+   |  +-----------------------------+
< |                 / ^     / ^     v                           |
< | .--------------'  |    /  |   +-----------------------+     |
< | |                 '   '   '---| Instruction Scheduler |     |
< | |\                 \  |       +-----------------------+     |
< | | v aTask           \ v cRead                    jTask ^    |
< | |  +-----+ cWrite +-----+     +-----------------------+ \   |
< | |  |     |------->|     |<----| Core Memory           |  '  |
< | |  | ALU |        |  C  |     |                       |  |  |
< | |  |     |        |  R  |     | +--------------------------------+
< | |\ +-----+        |  B  |     | | Cache / Data memory            |
< | | v aTask         |     |     | +--------------------------------+
< | |  +-----+ cWrite |     |     |                       |  |  |
< | |  |     |------->|     |     | +------------------+  |  |  |
< | |  | ALU |        |     |     | | Thread registers |  |  |  |
< | |  |     |        |     |     | +------------------+  |  |  |
< | |\ +-----+        |     |     +-----------------------+  |  |
< | | v aTask         |     |                        mTask ^ |  |
< | |  +-----+ cWrite |     |                               \|  |
< | |  |     |------->|     |                                |  |
< | |  | ALU |        |     |                                |  |
< | |  |     |        |     |                                |  |
< | |  +-----+        +-----+                                |  |
< | '--------------------------------------------------------'  |
< +-------------------------------------------------------------+

> core = undefined
> 	where
> 		iMemOut       = instrMem iPtr
> 		(instr, iPtr) = scheduler iMemOut
> 		cRead         = fetchArgs instr
> 		cOutput       = crb cRead cWrite
> 		aTask         = dispatch instr cOutput
> 		cWrite        = alu aTask
