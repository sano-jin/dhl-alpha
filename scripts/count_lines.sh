#!/bin/bash
# count the lines of the program

# echo "counting '"$1"'"
# find $1 | grep .*'.ml$' | grep -v main.ml | grep -v .*debug.*'.ml$' | xargs wc -l

find $1 | grep -v .*debug.*'.ml$' | xargs wc -l
# find $1 | grep -v main.ml | grep -v .*debug.*'.ml$' | xargs wc -l
