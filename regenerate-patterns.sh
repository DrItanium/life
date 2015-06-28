#!/bin/bash


files=(acorn beacon glider glider-multi gun0 line r-pentomino)

for f in "${files[@]}"
do
	echo "Regenerating $f"
	./generate-pattern.sh "data/patterns/inputs/$f" > "data/patterns/$f.clp"
done

