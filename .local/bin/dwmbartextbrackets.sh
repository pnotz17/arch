#!/usr/bin/env bash

while true; do
   
TEMP=$(sensors | grep Core | awk '{print substr($3, 2, length($3)-5)}' | tr "\\n" " " | sed 's/ /°C  /g' | sed 's/  $//')
DATETIME=`date`
MEM=$(free -h --kilo | awk '/^Mem:/ {print $3 "/" $2}')
CPU=$(top -bn1 | grep "Cpu(s)" | sed "s/.*, *\([0-9.]*\)%* id.*/\1/" | awk '{print 100 - $1"%"}')
VOL="$(amixer get Master | tail -n1 | sed -r 's/.*\[(.*)%\].*/\1/')" 

xsetroot -name "[CPU $CPU] [MEM $MEM] [VOL $VOL] [$DATETIME]"

        sleep 5s
done &
