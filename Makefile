
all:
	dune build @install @runtest

.PHONY: test
test:
	dune runtest

doc-html:
	dune build @doc

clean:
	dune clean
