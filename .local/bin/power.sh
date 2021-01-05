#!/bin/bash

RET=$(echo -e "shutdown\nreboot\nlogout\ncancel" | dmenu -l 5 -p "Logout")

case $RET in
	shutdown) sudo poweroff ;;
	reboot) sudo reboot ;;
	logout) pkill -KILL -u panos21 ;;
	*) ;;
esac
