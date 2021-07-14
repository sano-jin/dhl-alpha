#!/bin/bash
# Generate document using ocamldoc

cd "`dirname "$0"`"'/..' # `cd` to the project root directory

dune build @doc
dune build @doc-private

cp -r -f _build/default/_doc/_html/* docs/ocamldoc/

echo "done"


