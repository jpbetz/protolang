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

BUILDDIR=build


# Since we're testing a compilation workflow, keep intermediate .ll and .s file around for inspection
.SECONDARY:

test: $(BUILDDIR)/polynomial.result $(BUILDDIR)/minimal.result $(BUILDDIR)/fibonacci.result $(BUILDDIR)/willnotcompile.error
	diff $(BUILDDIR)/polynomial.result polynomial.expected
	diff $(BUILDDIR)/minimal.result minimal.expected
	diff $(BUILDDIR)/fibonacci.result fibonacci.expected
	diff $(BUILDDIR)/willnotcompile.error willnotcompile.expected
	@echo "TESTS PASSED"

$(BUILDDIR)/%.error: %.proto
	$(PROTO) $< 2> $@

$(BUILDDIR)/%.result: $(BUILDDIR)/%.out
	-./$< > $@  # - at beginning is to ignore exit status code

# Our language (.proto) to LLVM (.ll)
$(BUILDDIR)/%.ll: %.proto
	$(PROTO) $< > $@

# LLVM (,ll) to assembly code (.s)
$(BUILDDIR)/%.s: $(BUILDDIR)/%.ll
	$(LLC) $<

# Builds an executable from asembly code (.s), mixing in builtin external functions
$(BUILDDIR)/%.out: $(BUILDDIR)/%.s
	$(CC) $(BUILTINS) $< -o $@

clean:
	rm -rf $(BUILDDIR)/*
