simulate: *.lhs *.dat
	clash -fno-warn-tabs Main.lhs -O3 -o simulate

verilog: *.lhs *.dat
	clash -fno-warn-tabs Main.lhs --verilog

clean:
	@- rm *.hi *.o
