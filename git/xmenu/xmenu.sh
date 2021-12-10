#!/bin/sh

#cat <<EOF | xmenu -r | sh >$HOME/log 2>&1 &
cat <<EOF | xmenu -r | sh &
File manager	spacefm
Terminal 	st
Web browser	qutebrowser
Accessories
	feh	feh ~/multi/wallpapers/*
	sxiv	sxiv -t  ~/multi/wallpapers/*
	vim	st -e vim
Development
	geany	geany
Graphics
	gimp	gimp
Network
	waterfox	/home/pnotz17/.local/bin/waterfox
	mutt	st -e mutt
	transmission	transmission-gtk
Multimedia
	alsa mixer	st -e alsamixer
Office
Settings 
	customize Look & Feel	lxappearance
System 
	top	st -e top
Kill	xkill

Leave
	Exit		pkill -KILL -u pnotz17
	Reboot			doas reboot
	Shutdown		doas poweroff
EOF


