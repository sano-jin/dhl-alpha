#!/bin/bash

cd "`dirname "$0"`"'/..'
test () {
    echo "- generating $1"
    opam exec -- dune exec dhl -- example/$1.dhl -t > test/expected/expected_output_$1.log
}
echo "generating test output..."

test append
test test
test test2
test test3
test test4
test test5

echo "done"
