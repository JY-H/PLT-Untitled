
default: scanner parser ast

scanner:
	ocamllex scanner.mll

parser:
	ocamlyacc parser.mly

ast:
	ocamlc -c ast.m

.PHONY: all
all: clean scanner parser

.PHONY: clean
clean:
	rm -f scanner.ml parser.mli parser.ml *.output *.cmo *.cmi
