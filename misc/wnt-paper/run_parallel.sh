# Start tmux workers to execute the work in work.txt

dune build; dune install
sh ../../parallel.sh wnt work.txt -q all.katie -t traces/rep_0_trace_event/trace.json --profile