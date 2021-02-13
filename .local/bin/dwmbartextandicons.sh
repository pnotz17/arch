#!/bin/sh

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

pkgs() {
	pkgs=$(pacman -Q  |  wc -l)
	echo pkg: "  $pkgs"
}

updates() {
	updates=$(checkupdates 2> /dev/null | wc -l )
	echo pcm: "  $updates"
}

weather() {
	weather=$(curl 'https://wttr.in/Florina,Greece?format=%t')
	echo wea: "  $weather"
}

clock() {
	time=$(date +"%b %d, %R")
	echo "  $time"
}

alsa() {
	volume_alsa=$(amixer -M sget Master | grep Mono: | awk '{ print $2 }')
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

netspeed() {
	case $BLOCK_BUTTON in
	1) setsid -f "$TERMINAL" -e bmon ;;
	3) notify-send "🌐 Network traffic module" "🔻: Traffic received
🔺: Traffic transmitted" ;;
	6) "$TERMINAL" -e "$EDITOR" "$0" ;;
esac

update() {
    sum=0
    for arg; do
        read -r i < "$arg"
        sum=$(( sum + i ))
    done
    cache=${XDG_CACHE_HOME:-$HOME/.cache}/${1##*/}
    [ -f "$cache" ] && read -r old < "$cache" || old=0
    printf %d\\n "$sum" > "$cache"
    printf %d\\n $(( sum - old ))
}

rx=$(update /sys/class/net/[ew]*/statistics/rx_bytes)
tx=$(update /sys/class/net/[ew]*/statistics/tx_bytes)

printf "up:    %4sB\\n | do:   %4sB" $(numfmt --to=iec $tx) $(numfmt --to=iec $rx) 
}

main() {
	while true; do
		xsetroot -name "$(updates) | $(cpufrequency) | $(ram) | $(alsa) | $(netspeed) | $(clock) |"
		sleep 2
	done
}

main
