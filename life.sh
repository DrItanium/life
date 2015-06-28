#!/bin/bash

if [ $# -eq 0 ]; then
	echo "No patterns provided"
else
    pattern=$1
    shift
    maya-unicornhat -f2 src/life.clp -f2 $pattern $@ -f2 src/fragments/reset-run-exit.clp
fi
