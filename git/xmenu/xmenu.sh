#!/bin/sh

#cat <<EOF | xmenu -r | sh >$HOME/log 2>&1 &
cat <<EOF | xmenu -r | sh &
Apps
	spacefm
	st
	firefox	firefox	
	feh	feh ~/multi/wallpapers/*
	sxiv	sxiv -t  ~/multi/wallpapers/*
	vim	st -e vim
	geany	geany
	gimp	gimp
System 
	look & feel	lxappearance
	top	st -e top
	alsamixer	st -e alsamixer
Exit
	kill	xkill
	killx	pkill -KILL -u $USER
	reboot	doas reboot
	shutdown	doas poweroff
EOF


