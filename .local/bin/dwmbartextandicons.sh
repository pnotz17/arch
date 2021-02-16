#!/bin/sh
	
updates() {
	updates=$(checkupdates 2> /dev/null | wc -l )
	echo pcm: "  $updates"
}

pkgs() {
	pkgs=$(pacman -Q  |  wc -l)
	echo pkg: "  $pkgs"
}

weather() {
	weather=$(curl 'https://wttr.in/Florina,Greece?format=%t')
	echo wea: "  $weather"
}

cputemp() {
	temp ="$(sensors | grep Core | awk '{print substr($3, 2, length($3)-5)}' | tr "\\n" " " | sed 's/ /°C  /g' | sed 's/  $//')"
	echo "tem:   $cputemp"
}

cpufrequency() {
	read -r cpu a b c previdle rest < /proc/stat
	prevtotal=$((a+b+c+previdle))
	sleep 0.5
	read -r cpu a b c idle rest < /proc/stat
	total=$((a+b+c+idle))
	cpu=$((100*( (total-prevtotal) - (idle-previdle) ) / (total-prevtotal) ))
	echo cpu: "  $cpu"%
}

ram() {
	mem=$(free -h | awk '/Mem:/ { print $3 }' | cut -f1 -d 'i')
	echo mem: "  $mem"
}

upspeed() {
	T1=`cat /sys/class/net/enp2s0/statistics/tx_bytes`
	T2=`cat /sys/class/net/enp2s0/statistics/tx_bytes`
	TBPS=`expr $T2 - $T1`
	TKBPS=`expr $TBPS / 1024`
	echo -e "up:    $TKBPS KB/s"
}

downspeed() {
	R1=`cat /sys/class/net/enp2s0/statistics/rx_bytes`
	R2=`cat /sys/class/net/enp2s0/statistics/rx_bytes`
	RBPS=`expr $R2 - $R1`
	RKBPS=`expr $RBPS / 1024`
	echo -e "do:    $RKBPS KB/s"
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
			echo " $vol%"	
		fi
	fi
}

clock() {
	time=$(date +"%b %d, %R")
	echo "  $time"
}

while true; do
	xsetroot -name "$(cpufrequency) | $(ram) | $(alsa) | $(upspeed) | $(downspeed) | $(clock) |"
	sleep 1
done &



