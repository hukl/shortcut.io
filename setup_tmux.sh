#!/bin/sh

# start a new session, make it 256 color mode and detach from it
tmux -2 new-session -s shortcut -d

# create two more vertical splits
tmux split-window -v -t shortcut
tmux split-window -v -t shortcut

tmux send-keys -t shortcut:0.0 'make shell' C-m
tmux send-keys -t shortcut:0.1 'tail -f log/debug.log' C-m

tmux select-layout even-vertical

tmux attach -t shortcut
