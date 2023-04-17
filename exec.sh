#!/bin/bash

# An example script to execute KaTie
# Example usage: ./exec.sh test/large/catphos
# You need to have KaSim and KaTie in your PATH (run `dune install` for the latter)

DIR=$1 ; cd $DIR
echo "Running in directory: $DIR"

KaSim model.ka -seed 0 -trace trace.json -d kasim-output
echo -e "\n\n"
KaTie --debug-level 2 -t kasim-output/trace.json -q query.katie --output-dir katie-output