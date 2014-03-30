# Use the locally built proto
PROTO_HOME=..
PROTO=$(PROTO_HOME)/dist/build/proto/proto

# Use the local builtins
BUILTINS=$(PROTO_HOME)/c/builtins.c

# Make clang the default assembler
AS=clang

# Make clang the default c compiler
CC=clang

# LLVM
LLC=llc

# uncomment this line if you want to see intermediate .ll files that are generated
#.SECONDARY:

# Our language (.proto) to LLVM (.ll)
%.ll: %.proto
	$(PROTO) $< > $@

# LLVM (.ll) to assembly code (.s)
%.s: %.ll
	$(LLC) $<

# Builds an executable from asembly code (.s), mixing in builtin external functions
%: %.s
	$(CC) $(BUILTINS) $< -o $@
