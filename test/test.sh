#!/bin/bash

cd "`dirname "$0"`"'/..'
test () {
    echo "testing $1"
    opam exec -- dune exec dhl -- example/$1.dhl -t > test/tmp/output_$1.log
    opam exec -- patdiff test/tmp/output_$1.log test/expected/expected_output_$1.log || exit 1
}
echo testing
mkdir test/tmp
test append
test test
test test2
test test3
test test4
test test5

rm -rf test/tmp
