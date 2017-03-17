#!/bin/bash

parser=parser.mly;
testfile=tests/menhir_tests.txt;
comparefile=tests/menhir_compare.txt;
outfile=tests/menhir_output.txt;
menhir --interpret --interpret-show-cst ${parser} < ${testfile}  2> /dev/null | grep "ACCEPT\|REJECT" > ${outfile};
diff ${comparefile} ${outfile};
