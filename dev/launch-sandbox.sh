#! /usr/bin/env bash

# Launches several nodes using tmux.
#
# This *does not* start the docker container as loading it takes time,
# better keep it launched separatelly.
#
# If first time using tmux, some useful hotkeys:
# * Ctrl+B & y - exit
# * Ctrl+B <arrows> - change active pane
# * Ctrl+B PgDown/PgUp - freeze pane and scroll
#   + Q - unfreeze
#   + Ctrl+S - search
# * Ctrl+Z - zoom in / zoom out the active pane

mode=${1:-"all-mocks"}
session="Sandbox $mode"
panes=4

# Resize terminal
printf '\e[8;50;180t'

tmux new-session -d -s "$session"

for (( i = 1; i < panes; i++ )); do
    tmux split-window -v
done
tmux select-layout tiled

if [[ $mode = "all-mocks" ]]; then
    tmux select-pane -t 0
    tmux send-keys "# Running mock-app-backend" C-m
    tmux send-keys "stack exec mock-app-backend-exe" C-m
    tmux select-pane -t 1
    tmux send-keys "# Running gateway" C-m
    tmux send-keys "stack exec beckn-gateway-exe" C-m
    tmux select-pane -t 2
    tmux send-keys "# Running mock-provider-backend" C-m
    tmux send-keys "stack exec mock-provider-backend-exe" C-m

    tmux select-pane -t 3

elif [[ $mode = "fmd" ]]; then
    tmux select-pane -t 0
    tmux send-keys "# Running mock-app-backend" C-m
    tmux send-keys "stack exec mock-app-backend-exe" C-m
    tmux select-pane -t 1
    tmux send-keys "# Running gateway" C-m
    tmux send-keys "stack exec beckn-gateway-exe" C-m
    tmux select-pane -t 2
    tmux send-keys "# Running fmd-wrapper" C-m
    tmux send-keys "stack exec fmd-wrapper-exe" C-m

    tmux select-pane -t 3

else
    echo "Invalid mode $mode"
    tmux kill-session -t "$session"
fi

tmux attach-session -t "$session"
