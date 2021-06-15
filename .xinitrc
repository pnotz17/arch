#!/bin/sh

# X settings
xrandr --output VGA1 --mode 1408x792_60.00 --scale-from 1920x1080 &
xrdb -merge $HOME/.Xdefaults
xset r rate 400 100

# dbus
if test -z "$DBUS_SESSION_BUS_ADDRESS" ; then
 eval `dbus-launch --sh-syntax`
 echo "D-Bus per-session daemon address is: $DBUS_SESSION_BUS_ADDRESS"
fi

# Autostart
~/.local/bin/bars/dwm_bar_emoji &
~/.local/bin/xsession_destroyah &
~/.fehbg &
xclickroot -r ~/git/xmenu/xmenu.sh &
dunst &

#dwm start loop
while true; do
    /usr/local/bin/dwm 2> /dev/null
done


