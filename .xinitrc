#!/bin/sh

# X settings
xrandr --output VGA1 --mode 1408x792_60.00 --scale-from 1920x1080 &
xrdb -merge $HOME/.Xdefaults
xset dpms 600 900 1200
xset r rate 350 60

# Autostart
~/.local/bin/dwmbaremojis.sh &
~/.local/bin/xsessionautoremover.sh &
~/.fehbg &
xclickroot -r ~/git/xmenu/xmenu.sh &
picom -b &
dunst &

# Keymap
setxkbmap "us,gr" -option "grp:alt_shift_toggle"

#dwm restart loop
while true; do
    /usr/local/bin/dwm 2> /dev/null
done

exec dwm
