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

test: $(BUILDDIR)/polynomial.result $(BUILDDIR)/minimal.result $(BUILDDIR)/fibonacci.result
	diff $(BUILDDIR)/polynomial.result polynomial.expected
	diff $(BUILDDIR)/minimal.result minimal.expected
	diff $(BUILDDIR)/fibonacci.result fibonacci.expected
	@echo "TESTS PASSED"

$(BUILDDIR)/%.result: $(BUILDDIR)/%.out
	./$< > $@

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