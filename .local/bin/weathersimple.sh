#!/bin/bash

 weather="${XDG_DATA_HOME:-$HOME/.cache/}weather"

 getweather="$(curl -sf 'https://wttr.in/YOURCITY,YOURCOUNTRY?format=%t' > "$weather" || exit 1 ;)"

 showweather() { cat "$weather" ;} 

 case $BLOCK_BUTTON in
                 1) setsid -f "$TERMINAL" -e less -Srf "$weatherreport" ;;
                 2) getweather && showweather ;;
                 3) notify-send "�� Weather module" "\- Left click for full forecast.
                 - Middle click to update forecast." ;;
                 6) "$TERMINAL" -e "$EDITOR" "$0" ;;
         esac

 #printf "%s" "$weather"

 [ "$(stat -c %y "$weather" 2>/dev/null | cut -d' ' -f1)" = "$(date '+%Y-%m-%d')" ] ||
                 getweather

 showweather
