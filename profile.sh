# Profiling script based on perf, to be used on Linux
#
# After installing perf, you may have to run the following:
#     echo 0 | sudo tee /proc/sys/kernel/perf_event_paranoid
#     echo 0 | sudo tee /proc/sys/kernel/kptr_restrict
#
# Reference: https://github.com/ocaml-bench/notes/blob/master/profiling_notes.md

DIR=$1 ; shift ; cd $DIR
echo "Running in directory: $DIR"

rm -rf kasim-output katie-output
KaSim model.ka -seed 0 -trace trace.json -d kasim-output
echo -e "\n\n"

rm perf.data
perf record -g --call-graph=dwarf -- KaTie -t kasim-output/trace.json -q query.katie --output-dir katie-output
perf script > perf.out
rm perf.data

# Then, perf.out can be visualized with `profiler.firefox.com`