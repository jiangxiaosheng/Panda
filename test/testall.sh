#!/usr/bin/bash

globallog=testall.log
rm -f $globallog
keep=1
error=0
globalerror=0

PANDA="../_build/default/bin/main.exe"

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Compare() {
#     generatedfiles="$generatedfiles $3"
#     echo diff -b $1 $2 ">" $3 1>&2
#     diff -b "$1" "$2" > "$3" 2>&1 || {
# 	SignalError "$1 differs"
# 	echo "FAILED $1 differs from $2" 1>&2
#     }
# }

Run() {
    echo $* 1>&2
    eval $* || {
		SignalError "$1 failed on $*"
		return 1
    }
}

RunFail() {
    echo $* 1>&2
    eval $* && {
		SignalError "failed: $* did not report an error"
		return 1
    }
    return 0
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.pd//'`
    reffile=`echo $1 | sed 's/.pd$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    Run "$PANDA" "<" $1 ">" "$basedir/${basename}.out" &&

    if [ $error -eq 0 ] ; then
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

CheckFail() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.pd//'`
    reffile=`echo $1 | sed 's/.pd$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    RunFail "$PANDA" "<" $1 "2>" "$basedir/${basename}.err" ">>" $globallog &&

    if [ $error -eq 0 ] ; then
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}



if [ $# -ge 1 ]
then
    files=$@
else
    files="tests/test-*.pd tests/fail-*.pd"
fi

for file in $files
do
    case $file in
	*test-*)
	    Check $file 2>> $globallog
	    ;;
	*fail-*)
	    CheckFail $file 2>> $globallog
	    ;;
	*)
	    echo "unknown file type $file"
	    globalerror=1
	    ;;
    esac
done

exit $globalerror