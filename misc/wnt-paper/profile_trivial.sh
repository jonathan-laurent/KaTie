# Profile the trivial query using perf

rm -f perf.data
perf record -g --call-graph=dwarf -- dune exec KaTie -- -q trivial.katie -t traces/rep_0_trace_event/trace.json --output-dir out
perf script > perf.out
rm -f perf.data
