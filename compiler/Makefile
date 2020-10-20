.POSIX:

# FIXME Everything is an error except wildcard pattern matching.
OFLAGS =  -w A -warn-error A

all: myrnac

myrnac: parser.ml lexer.ml dump.ml fe.ml me.ml driver.ml
	ocamlopt $(OFLAGS) -o $@ parser.ml lexer.ml dump.ml fe.ml me.ml driver.ml

debug: myrnac-debug

myrnac-debug: parser.ml lexer.ml dump.ml fe.ml me.ml driver.ml
	ocamlc $(OFLAGS) -g -o $@ parser.ml lexer.ml dump.ml fe.ml me.ml driver.ml

lexer.ml: lexer.mll
	ocamllex $?

parser.ml: parser.mly
	ocamlyacc parser.mly
	rm -f parser.mli

clean:
	rm -f driver.cmi driver.cmx driver.cmo driver.o
	rm -f lexer.cmi lexer.cmx lexer.cmo lexer.o lexer.ml
	rm -f parser.mli parser.ml parser.cmi parser.cmx parser.cmo parser.o
	rm -f me.cmi me.cmx me.cmo me.o
	rm -f fe.cmi fe.cmx fe.cmo fe.o
	rm -f dump.cmi dump.cmx dump.cmo dump.o
	rm -f myrnac myrnac-debug
