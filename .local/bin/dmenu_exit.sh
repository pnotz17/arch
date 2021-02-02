#!/bin/bash

RET=$(echo -e "shutdown\nreboot\nlogout\ncancel" | dmenu -l 5 -p "Logout")

case $RET in
	shutdown) shutdown now ;;
	reboot)  reboot now ;;
	logout) pkill -KILL -u yourusername ;;
	*) ;;
esac
