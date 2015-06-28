#!/bin/bash

if [ $# -eq 0 ]; then
	echo "No patterns provided"
	return 1
elif [ $# -eq 1 ]; then 
	file="/tmp/$RANDOM.clp"
	echo "(open $1 input \"r\")" > $file
	echo "(generate-board $1 input)" >> $file
	maya-unicornhat -f2 src/tools/pattern-generator.clp -f2 $file -f2 src/fragments/reset-run-exit.clp
	rm $file
else
	file="/tmp/$RANDOM.clp"
	echo "(open $1 input \"r\")" > $file
	echo "(open $2 output \"w\")" >> $file
	echo "(generate-board $1 input output)" >> $file
	maya-unicornhat -f2 src/tools/pattern-generator.clp -f2 $file -f2 src/fragments/reset-run-exit.clp
	rm $file
fi
