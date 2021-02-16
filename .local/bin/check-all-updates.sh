#!/bin/sh

if ! pacmanupdates="$(checkupdates 2> /dev/null | wc -l)"; then
    updates_arch=0
fi

# if ! updates_aur=$(cower -u 2> /dev/null | wc -l); then
if ! aurupdates="$(yay -Qum 2> /dev/null | wc -l)"; then
    updates_aur=0
fi

updates=$(("$pacmanupdates" + "$aurupdates"))

if [ "$updates" -gt 0 ]; then
    echo " System Updates: $updates"
else
    echo ""
fi

