#!/bin/bash

while true; do
    open=$(ls -1a ~/.local/bin --file-type | dmenu -c -g 1 -l 80 "$@")
    if [[ -d "$open" ]]; then
        cd "$open"
    else
        if [[ "$open" != "" ]]; then
            xdg-open "$open"
        fi
        exit 0
    fi
done
