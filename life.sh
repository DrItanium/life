#!/bin/bash

if [ $# -eq 0 ]; then
	echo "No patterns provided"
else
    pattern=$1
    shift
    maya-unicornhat -f2 life.clp -f2 $pattern $@ -f2 fragments/reset-run-exit.clp
fi
