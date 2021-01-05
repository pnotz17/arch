!/bin/sh
#cputemp()
#{
	#CPU_TEMP="$(sensors | grep Core | awk '{print substr($3, 2, length($3)-5)}' | tr "\\n" " " | sed 's/ /°C  /g' | sed 's/  $//')"
	#PREFIX='temp: '

	#if [ "$CPU_TEMP" -ge $WARNING_LEVEL ]; then
		#PREFIX="$PREFIX"
	#fi
	#echo "$PREFIX$CPU_TEMP"
#}
cpu() {
	read -r cpu a b c previdle rest < /proc/stat
	prevtotal=$((a+b+c+previdle))
	sleep 0.5
	read -r cpu a b c idle rest < /proc/stat
	total=$((a+b+c+idle))
	cpu=$((100*( (total-prevtotal) - (idle-previdle) ) / (total-prevtotal) ))
	echo cpu:" $cpu"%
}
ram() {
	mem=$(free -h | awk '/Mem:/ { print $3 }' | cut -f1 -d 'i')
	echo mem:" $mem"
}
volume_alsa() {

	mono=$(amixer -M sget Master | grep Mono: | awk '{ print $2 }')

	if [ -z "$mono" ]; then
		muted=$(amixer -M sget Master | awk 'FNR == 6 { print $7 }' | sed 's/[][]//g')
		vol=$(amixer -M sget Master | awk 'FNR == 6 { print $5 }' | sed 's/[][]//g; s/%//g')
	else
		muted=$(amixer -M sget Master | awk 'FNR == 5 { print $6 }' | sed 's/[][]//g')
		vol=$(amixer -M sget Master | awk 'FNR == 5 { print $4 }' | sed 's/[][]//g; s/%//g')
	fi

	if [ "$muted" = "off" ]; then
		echo Vol "muted"
	else
		if [ "$vol" -ge 65 ]; then
			echo "vol: $vol%"
		elif [ "$vol" -ge 40 ]; then
			echo "vol: $vol%"
		elif [ "$vol" -ge 0 ]; then
			echo "vol: $vol%"	
		fi
	fi
}
clock() {
	datetime=`date`


	echo "$datetime"
}
main() {
	while true; do
		xsetroot -name " $(cputemp) | $(cpu) | $(ram) | $(volume_alsa) | $(clock) |"
		sleep 1
	done
}

main
