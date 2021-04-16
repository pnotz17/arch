#!/bin/bash
# xsel, calc  REQUIRED
while inp=$(echo -e "$oup" | dmenu -p Calculate:)
do
    if oup2=$(calc -pd "$inp"); then
        echo -n "$oup2" | xsel -i
        oup="$oup2\n$oup"
    fi
done
