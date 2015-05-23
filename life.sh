#!/bin/bash

if [ $# -eq 0 ]; then
	echo "No patterns provided"
else
	maya-unicornhat -f2 life.clp -f2 $1 -f1 fragments/reset-run-exit.clp
fi
