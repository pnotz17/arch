#!/bin/sh

updates() {
	updates=$(checkupdates 2> /dev/null | wc -l )
	echo pcm: "$updates"
}

pkgs() {
	pkgs=$(pacman -Q  |  wc -l)
	echo pkg: "$pkgs"
}

weather() {
	weather=$(curl 'https://wttr.in/Florina,Greece?format=%t')
	echo wea: "$weather"
}

cputemp() {
	temp ="$(sensors | grep Core | awk '{print substr($3, 2, length($3)-5)}' | tr "\\n" " " | sed 's/ /°C  /g' | sed 's/  $//')"
	echo "tem: $cputemp"
}

cpufrequency() {
	read -r cpu a b c previdle rest < /proc/stat
	prevtotal=$((a+b+c+previdle))
	sleep 0.5
	read -r cpu a b c idle rest < /proc/stat
	total=$((a+b+c+idle))
	cpu=$((100*( (total-prevtotal) - (idle-previdle) ) / (total-prevtotal) ))
	echo cpu: "$cpu"%
}

ram() {
	mem=$(free -h | awk '/Mem:/ { print $3 }' | cut -f1 -d 'i')
	echo mem: "$mem"
}

alsa() {
    volume="$(amixer get Master | tail -n1 | sed -r 's/.*\[(.*)%\].*/\1/')" 
	echo "vol: $volume"
}

upspeed() {
T1=`cat /sys/class/net/enp2s0/statistics/tx_bytes`
sleep 1
T2=`cat /sys/class/net/enp2s0/statistics/tx_bytes`
TBPS=`expr $T2 - $T1`
TKBPS=`expr $TBPS / 1024`
echo -e "up:  $TKBPS KB/s"
}

downspeed() {
R1=`cat /sys/class/net/enp2s0/statistics/rx_bytes`
sleep 1
R2=`cat /sys/class/net/enp2s0/statistics/rx_bytes`
RBPS=`expr $R2 - $R1`
RKBPS=`expr $RBPS / 1024`
echo -e "do:  $RKBPS KB/s"
}

clock() {
	time=$(date +"%b %d, %R")
	echo "$time"
}

main() {
	while true; do
		xsetroot -name "$(updates) | $(cpufrequency) | $(ram) | $(alsa) | $(upspeed) | $(downspeed) | $(clock) |"
		sleep 2
	done
}
main
