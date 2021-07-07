#!/bin/bash

test () {
    echo "testing $1"
    opam exec -- dune exec dhl -- ../example/$1.dhl -t > expected/expected_output_$1.log
}
echo testing
test append
test test
test test2
test test3
test test4
test test5


