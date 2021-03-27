#!/bin/bash

RET=$(echo -e "shutdown\nreboot\nlogout\ncancel" | dmenu -l 5 -p "Logout")

case $RET in
	shutdown)  poweroff ;;
	reboot)  reboot ;;
	logout)  pkill -KILL -u $USER ;;
	*) ;;
esac
