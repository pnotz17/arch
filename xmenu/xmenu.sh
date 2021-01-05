#!/bin/sh

#cat <<EOF | xmenu -r | sh >$HOME/log 2>&1 &
cat <<EOF | xmenu -r | sh &
File manager	pcmanfm
Terminal 	st
Web browser	firefox	
Accessories
	feh	feh  ~/Pictures/Wallpapers/*
	vim	st -e vim
Development
	geany	geany
	keepassxc	keepassxc
Graphics
	gimp	gimp
	xsane-scanning	xsane
Network 
	qutebrowser	/usr/bin/qutebrowser
	firefox	firefox
	transmission	transmission-gtk
Multimedia
	alsa mixer	st -e alsamixer
	mpv	mpv
Office
Settings 
	Customize Look & Feel	lxappearance
System 
	htop	st -e htop
Kill	xkill

Leave
	Exit		pkill -KILL -u panos21
	Reboot			reboot
	Shutdown		poweroff
EOF


