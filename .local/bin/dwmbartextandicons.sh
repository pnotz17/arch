#!/bin/sh

mail() {
	mail=`curl -su pnotz17:KaDfYnUyjb64XGK https://mail.google.com/mail/feed/atom || echo "<fullcount>unknown number of</fullcount>"`
	mail=`echo "$mail" | grep -oPm1 "(?<=<fullcount>)[^<]+" `
	echo  "inb:   $mail"
}

updates() {
	updates=$(checkupdates 2> /dev/null | wc -l )  
	echo "pcm:   $updates"
}

pkgs() {
	pkgs=$(pacman -Q  |  wc -l)
	echo "pkg:   $pkgs"
}

weather() {
	weather=$(curl 'https://wttr.in/Florina,Greece?format=%t')
	echo "wea: ️  $weather"
}

cputemp() {
	cputemp=$(sensors | grep Core | awk '{print substr($3, 2, length($3)-5)}' | tr "\\n" " " | sed 's/ /°C  /g' | sed 's/  $//')
	echo "tem:   $cputemp"
}

cpufrequency() {
	cpu=$(awk '{u=$2+$4; t=$2+$4+$5;if (NR==1){u1=u; t1=t;} else printf("%d%%", ($2+$4-u1) * 100 / (t-t1) "%");}' <(grep 'cpu ' /proc/stat) <(sleep 0.5; grep 'cpu ' /proc/stat))
	echo "cpu:   $cpu"%
}

ram() {
	mem=$(free -h | awk '/Mem:/ { print $3 }' | cut -f1 -d 'i')
	echo "mem:   $mem"
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
		echo "vol:   muted"
	else
		if [ "$vol" -ge 65 ]; then
			echo "vol:   $vol%"
		elif [ "$vol" -ge 40 ]; then
			echo "vol: 墳  $vol%"
		elif [ "$vol" -ge 0 ]; then
			echo"vol:   $vol%"	
		fi
	fi
}

upspeed() {
	T1=`cat /sys/class/net/enp2s0/statistics/tx_bytes`
	sleep 1
	T2=`cat /sys/class/net/enp2s0/statistics/tx_bytes`
	TBPS=`expr $T2 - $T1`
	TKBPS=`expr $TBPS / 1024`
	printf  "up:    $TKBPS kb"
}

downspeed() {
	R1=`cat /sys/class/net/enp2s0/statistics/rx_bytes`
	sleep 1
	R2=`cat /sys/class/net/enp2s0/statistics/rx_bytes`
	RBPS=`expr $R2 - $R1`
	RKBPS=`expr $RBPS / 1024`
	printf  "do:    $RKBPS kb"
}

clock() {
	time=$(date +"%b %d,%R")
	echo "  $time"
}

while true; do
	xsetroot -name "^c#FFA500^$(mail)  |  ^c#25E80D^$(updates)  |  ^c#00FFF9^$(cpufrequency)  |  ^c#D400CF^$(ram)  |  ^c#ADD8E6^$(upspeed)  |  ^c#FFC0CB^$(downspeed)  |  ^c#FFFFFF^$(alsa)  |  ^c#FFFF00^$(clock)  |"	#xsetroot -name "$(updates) | $(cpufrequency) | $(ram) | $(alsa) | $(upspeed) | $(downspeed) | $(clock) |"
	sleep 2
done &


