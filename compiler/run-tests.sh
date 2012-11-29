#!/bin/sh

PBJC="./pbjc -c "

files="test/*.pbj"

CleanUp() {
	rm test.out test.null
}

GetError() {
	eval "$PBJC $1" 2> test.out 1> test.null
	wc -l test.out | awk '{print $1}'
}

for file in $files
do
	errors=$(GetError $file)
	GetError $file
	case $file in
		*fail*)
			if [ $errors -gt 0 ]
			then
				echo "Failure test: " $file " passed."
			else
				echo "Failure test: " $file " failed to fail."
				CleanUp
				exit 1
			fi
			;;
		*)
			if [ $errors -eq 0 ]
			then 
				echo "Test: " $file " passed."
			else
				echo $file " failed to pass."
				CleanUp
				exit 1
			fi
			;;
	esac
done
CleanUp
echo "All tests passed.\n"
