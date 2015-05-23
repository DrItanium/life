#!/bin/bash

if [ $#=0 ];
then
	echo "No patterns provided"
else
	maya-unicornhat -f2 life.clp -f2 $1 -f2 fragments/exit.clp
fi
