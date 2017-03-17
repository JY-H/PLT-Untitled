#!/bin/bash

DECAF_PATH=~/DECAF

parser=${DECAF_PATH}/parser.mly;
testfile=${DECAF_PATH}/tests/menhir/menhir.tests
comparefile=${DECAF_PATH}/tests/menhir/menhir.compare
outfile=${DECAF_PATH}/tests/menhir/menhir.output

menhir --interpret --interpret-show-cst ${parser} < ${testfile} 2> /dev/null | grep "ACCEPT\|REJECT" > ${outfile};
compare=$(diff ${comparefile} ${outfile} | grep -o '^[1-9]');

if [[ "${compare}" != "" ]]; then
	echo "Failed:";
	while not_done='\n' read -ra linenos; do
		for lineno in "${linenos[@]}"; do
			echo -n "Test #${lineno}: ";
			sed ${lineno}'!d' ${testfile};
		done
	done <<< "${compare}"
else
	echo "All menhir tests passed.";
fi
