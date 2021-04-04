#!/bin/bash

while true; do
    open=$(ls -1a --group-directories-first --file-type | dmenu -l 6 "$@")
    if [[ -d "$open" ]]; then
        cd "$open"
    else
        if [[ "$open" != "" ]]; then
            xdg-open "$open"
        fi
        exit 0
    fi
done
