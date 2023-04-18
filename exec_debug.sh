#!/bin/bash

# This script is similar to 'exec.sh' but it uses 'dune exec' to allow for
# debugging the source code of KaSim and KaTie simultaneously.
# To do so, just clone both repos in the same parent folder and run this script from
# the parent folder.

DIR=$1 ; shift ; cd $DIR
echo "Running in directory: $DIR"

rm -rf kasim-output katie-output
dune exec KaSim -- model.ka -seed 0 -trace trace.json -d kasim-output
echo -e "\n\n"
dune exec KaTie -- -t kasim-output/trace.json -q query.katie --output-dir katie-output "$@"