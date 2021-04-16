#!/bin/sh
# A fuzzy file-finder and opener based on dmenu
# Requires: dmenu, xdg-open
find ~/.local/bin | sed 's/ /\\ /g' | sort -f | dmenu -c -g 1 -l 50 | xargs xdg-open
