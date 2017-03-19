
default: scanner.ml parser ast sast semant parser.cmo parser.cmo

scanner.cmo: scanner.ml
	ocamlc -c scanner.ml

parser.cmo: parser.mli
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml

parser.ml: parser.mly
	ocamlyacc parser.mly

semant: semant.ml
	ocamlc -c semant.ml

sast: sast.ml
	ocamlc -c sast.ml

ast: ast.ml
	ocamlc -c ast.ml

parser: parser.mly
	ocamlyacc -v parser.mly

scanner.ml: scanner.mll
	ocamllex scanner.mll

.PHONY: menhir_test
menhir_test:
	./menhir_testall.sh

.PHONY: all
all: clean scanner parser

.PHONY: clean
clean:
	rm -f scanner.ml parser.mli parser.ml *.output *.cmo *.cmi tests/menhir/menhir.output
