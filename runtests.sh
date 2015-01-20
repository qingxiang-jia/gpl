#!/bin/bash

GPL="src/gpl"

# Set time limit for all operations
ulimit -t 30

successcount=0
errorcount=0

Check() {
    basename=`echo $1 | sed 's/.*\\///
                             s/.gpl//'`

    [ -f "test/$basename.err" ] && expectation=1 || expectation=0

    echo "###### Testing $basename"

    eval "$GPL <$* >/dev/null 2>&1" && actual=0 || actual=1

    if [[ ($expectation -eq 0) && ($actual -eq 0) ]] ; then
	eval "./a.out >output.txt"
	eval "diff -B --strip-trailing-cr output.txt test/$basename.out >/dev/null" && actual=0 || actual=1
    fi

    if [ $expectation -eq $actual ] ; then
    	echo "###### SUCCESS" 1>&2
	successcount=$((successcount+1))
    else
    	echo "###### FAILED" 1>&2
    	errorcount=$((errorcount+1))
    fi
}

if [ $# -eq 0 ]; then
	test_files="test/*.gpl"

	for test in $test_files
	do
		Check $test
	done

	if [ $errorcount -eq 0 ]; then
	    echo "All tests passed!"
	else
	    echo "$successcount passed; $errorcount failed."
	fi

	exit $errorcount

else
	Check "test/$1.gpl"
fi
