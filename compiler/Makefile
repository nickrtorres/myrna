.POSIX:

OFLAGS =  -w A -warn-error A
SOURCES = parser.ml lexer.ml dump.ml \
	fe.ml me.ml re.ml kleene.ml be.ml driver.ml

all: myrnac

myrnac: $(SOURCES)
	ocamlopt $(OFLAGS) -o $@ $(SOURCES)

check: tests
	./tests

tests: re.ml simplify.ml tests.ml
	ocamlopt -o $@ re.ml simplify.ml tests.ml

lexer.ml: lexer.mll
	ocamllex $?

parser.ml: parser.mly
	ocamlyacc parser.mly
	rm -f parser.mli

clean:
	rm -f be.cmi be.cmx be.cmo be.o
	rm -f driver.cmi driver.cmx driver.cmo driver.o
	rm -f dump.cmi dump.cmx dump.cmo dump.o
	rm -f fe.cmi fe.cmx fe.cmo fe.o
	rm -f kleene.cmi kleene.cmx kleene.cmo kleene.o
	rm -f lexer.cmi lexer.cmx lexer.cmo lexer.o lexer.ml
	rm -f me.cmi me.cmx me.cmo me.o
	rm -f parser.mli parser.ml parser.cmi parser.cmx parser.cmo parser.o
	rm -f re.cmi re.cmx re.cmo re.o
	rm -f simplify.cmi simplify.cmx simplify.cmo simplify.o
	rm -f tests.cmi tests.cmx tests.cmo tests.o
	rm -f myrnac myrnac-debug tests
