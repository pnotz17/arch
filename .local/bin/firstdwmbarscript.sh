#!/usr/bin/env bash
while true; do

        date '+%m-%d-%Y|%R ' > /tmp/CurTime.tmp

        sleep 60s
done &

while true; do
        Pkgs=$(checkupdates 2> /dev/null |pacman -Q  |  wc -l)
        PACMAN=$(checkupdates 2> /dev/null | wc -l )
        TEMP=$(sensors | grep Core | awk '{print substr($3, 2, length($3)-5)}' | tr "\\n" " " | sed 's/ /°C  /g' | sed 's/  $//')
        DISKROOT=$(df -Ph | grep "/dev/sda1" | awk {'print $5'})
        DISKHOME=$(df -Ph | grep "/dev/sda2" | awk {'print $5'})
        LOCALTIME=$(< /tmp/CurTime.tmp)
        DISKROOT=$(df -Ph | grep "/dev/sda1" | awk {'print $5'})
        DISKHOME=$(df -Ph | grep "/dev/sda2" | awk {'print $5'})
        MEM=$(free -h --kilo | awk '/^Mem:/ {print $3 "/" $2}')
        LINUX=$(uname -r)
        CPU=$(top -bn1 | grep "Cpu(s)" | sed "s/.*, *\([0-9.]*\)%* id.*/\1/" | awk '{print 100 - $1"%"}')
        WEATHER=$(curl wttr.in/Florina?format="%l:+%m+%p+%w+%t+%c+%C")
        VOL="$(amixer get Master | tail -n1 | sed -r 's/.*\[(.*)%\].*/\1/')" 
        xsetroot -name "|  UPDATES $PACMAN  |  TEMP $TEMP  |  CPU $CPU  |  MEM $MEM  |  VOL $VOL  |  $LOCALTIME |"

        sleep 10s
done &
