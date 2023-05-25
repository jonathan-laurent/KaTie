# Automatically start several parallel tmux sessions with KaTie
#
# Usage: sh parallel.sh <session-name> <spec> <katie-args>
#
# - session-name: name of the tmux session being created.
# - spec: name of a file specifyfing the work to do. Every line is a comma-separated list
#   of query names to be sent to a worker. There are as many lines as workers to spawn.
#   The spec file must end with a newline character.
# - katie-args: the rest of KaTie's arguments, to be passed as is. The --output-dir and
#   --only arguments are passed automatically.
#
# Ref: https://how-to.dev/how-to-create-tmux-session-with-a-script

session=$1; shift
spec=$1; shift

tmux new-session -d -s $session

worker=0
cat "$spec" | while IFS= read -r line
do
    worker=$((worker+1))
    echo "Worker $worker: $line"
    tmux new-window -t $session:$worker -n "W$worker"
    tmux send-keys -t $session:$worker "KaTie $* --output-dir output/$worker --only $line" Enter
done

tmux attach-session -t $session