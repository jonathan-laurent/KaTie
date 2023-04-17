#!/bin/bash

# An example script to execute KaTie
# Usage: ./exec.sh TEST_DIR [OTHER_KATIE_ARGS]
# Usage example: ./exec.sh tests/large/catphos/ --debug-level 2 --no-color
# You need to have KaSim and KaTie in your PATH (run `dune install` for the latter)

DIR=$1 ; shift ; cd $DIR
echo "Running in directory: $DIR"

rm -rf $DIR/kasim-output $DIR/katie-output
KaSim model.ka -seed 0 -trace trace.json -d kasim-output
echo -e "\n\n"
KaTie -t kasim-output/trace.json -q query.katie --output-dir katie-output "$@"