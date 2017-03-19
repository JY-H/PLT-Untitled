#!/bin/bash

DECAF_PATH=~/DECAF

parser=${DECAF_PATH}/parser.mly;
testfile=${DECAF_PATH}/tests/menhir/menhir.tests
comparefile=${DECAF_PATH}/tests/menhir/menhir.compare
outfile=${DECAF_PATH}/tests/menhir/menhir.output

menhir --interpret --interpret-show-cst ${parser} < ${testfile} 2> /dev/null | grep "ACCEPT\|REJECT" > ${outfile};
compare=$(diff <(nl ${comparefile}) <(nl ${outfile}) | grep -o '^[0-9]\+\(,[0-9]\+\)\?');

if [[ "${compare}" != "" ]]; then
	while not_done='\n' read -ra linerange; do
		for range in "${linerange[@]}"; do
			if [[ "${range}" =~ "," ]]; then
				IFS=',' read strt end <<< "${range}";
					for i in $(seq ${strt} ${end}); do
						echo -n "Test #${i}: ";
						sed ${i}'!d' ${testfile};
						echo
					done
			else
				echo -n "Test #${range}: ";
				sed ${range}'!d' ${testfile};
			fi
		done
	done <<< "${compare}"
else
	echo "All menhir tests passed.";
fi
