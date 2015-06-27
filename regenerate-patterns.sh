#!/bin/bash


files=(acorn beacon glider glider-multi gun0 line r-pentomino)

for f in "${files[@]}"
do
	echo "Regenerating $f"
	./generate-pattern.sh "patterns/inputs/$f" > patterns/$f.clp
done

