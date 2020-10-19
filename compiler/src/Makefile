.POSIX:

all: driver

driver: lexer.ml token.ml driver.ml
	ocamlopt -o $@ token.ml lexer.ml driver.ml

lexer.ml: lexer.mll
	ocamllex $?

clean:
	rm -f driver.cmi driver.cmx driver.o driver
	rm -f token.cmi token.cmx token.o
	rm -f lexer.cmi lexer.cmx lexer.o lexer.ml
