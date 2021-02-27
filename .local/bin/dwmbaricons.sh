#!/bin/sh

updates() {
	updates=$(checkupdates 2> /dev/null | wc -l )  
	echo "  $updates"
}

pkgs() {
	pkgs=$(pacman -Q  |  wc -l)
	echo "  $pkgs"
}

weather() {
	weather=$(curl 'https://wttr.in/Florina,Greece?format=%t')
	echo "️  $weather"
}

cputemp() {
	cputemp=$(sensors | grep Core | awk '{print substr($3, 2, length($3)-5)}' | tr "\\n" " " | sed 's/ /°C  /g' | sed 's/  $//')
	echo "  $cputemp"
}

cpufrequency() {
	cpu=$(awk '{u=$2+$4; t=$2+$4+$5;if (NR==1){u1=u; t1=t;} else printf("%d%%", ($2+$4-u1) * 100 / (t-t1) "%");}' <(grep 'cpu ' /proc/stat) <(sleep 0.5; grep 'cpu ' /proc/stat))
	echo "  $cpu"%
}

ram() {
	mem=$(free -h | awk '/Mem:/ { print $3 }' | cut -f1 -d 'i')
	echo "  $mem"
}

alsa() {
	mono=$(amixer -M sget Master | grep Mono: | awk '{ print $2 }')
	if [ -z "$mono" ]; then
		muted=$(amixer -M sget Master | awk 'FNR == 6 { print $7 }' | sed 's/[][]//g')
		vol=$(amixer -M sget Master | awk 'FNR == 6 { print $5 }' | sed 's/[][]//g; s/%//g')
	else
		muted=$(amixer -M sget Master | awk 'FNR == 5 { print $6 }' | sed 's/[][]//g')
		vol=$(amixer -M sget Master | awk 'FNR == 5 { print $4 }' | sed 's/[][]//g; s/%//g')
	fi

	if [ "$muted" = "off" ]; then
		echo "  muted"
	else
		if [ "$vol" -ge 65 ]; then
			echo "  $vol%"
		elif [ "$vol" -ge 40 ]; then
			echo "墳  $vol%"
		elif [ "$vol" -ge 0 ]; then
			echo "  $vol%"	
		fi
	fi
}

upspeed() {
	T1=`cat /sys/class/net/enp2s0/statistics/tx_bytes`
	sleep 1
	T2=`cat /sys/class/net/enp2s0/statistics/tx_bytes`
	TBPS=`expr $T2 - $T1`
	TKBPS=`expr $TBPS / 1024`
	printf  "  $TKBPS kb"
}

downspeed() {
	R1=`cat /sys/class/net/enp2s0/statistics/rx_bytes`
	sleep 1
	R2=`cat /sys/class/net/enp2s0/statistics/rx_bytes`
	RBPS=`expr $R2 - $R1`
	RKBPS=`expr $RBPS / 1024`
	printf  "  $RKBPS kb"
}

clock() {
	time=$(date +"%b %d, %R")
	echo "  $time"
}

while true; do
	xsetroot -name "$(updates) | $(cpufrequency) | $(ram) | $(alsa) | $(upspeed) | $(downspeed) | $(clock) |"
	sleep 2
done &


