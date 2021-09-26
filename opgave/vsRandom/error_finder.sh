#!/bin/bash

do_test() {
	teller=$1
	mkdir testresults 2> /dev/null
	node TestEngine.js > testresults/test$teller.log
	status=$?
	if [ "$status" -ne 0 ]; then
		echo "TEST $teller FAILED!"
		result=1
	else
		rm testresults/test$teller.log
		result=0
	fi
	return $result
}
rm -rf testresults

iterations=$1

if [ -z "$1" ]; then
	iterations=50
fi

for i in $(seq 1 $iterations); do
	do_test $i &
done

wait < <(jobs -p)
printf "TOTAL: FAILED "
ls testresults | wc -l | xargs printf
echo " TESTS"
