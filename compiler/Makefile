.POSIX:

all: driver

driver: parser.ml lexer.ml driver.ml
	ocamlopt -o $@ parser.ml lexer.ml driver.ml

lexer.ml: lexer.mll
	ocamllex $?

parser.ml: parser.mly
	ocamlyacc parser.mly
	rm -f parser.mli

clean:
	rm -f driver.cmi driver.cmx driver.o driver
	rm -f lexer.cmi lexer.cmx lexer.o lexer.ml
	rm -f parser.mli parser.ml parser.cmi parser.cmx parser.o parser
