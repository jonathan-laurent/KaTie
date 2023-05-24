# dune exec ../src/main.bc -- --no-progress-bars -q testing_query.katie -t rep_0_trace_event/trace.json --output-dir out

QUERY=trivial.katie
dune exec KaTie -- -q $QUERY -t traces/rep_0_trace_event/trace.json --output-dir out