
default: scanner parser

scanner:
	ocamllex scanner.mll

parser:
	ocamlyacc parser.mly

.PHONY: all
all: clean scanner parser

.PHONY: clean
clean:
	rm scanner.ml parser.mli parser.ml *.output
