#!/bin/sh

# X settings
xrandr --output VGA1 --mode 1408x792_60.00 --scale-from 1920x1080 &
xrdb -merge $HOME/.Xdefaults

# Autostart
~/.local/bin/dwmbaremojis.sh &
~/.local/bin/xsessionautoremover.sh &
~/.fehbg &
xclickroot -r ~/.config/suckless-git/xmenu/xmenu.sh &
picom -b &
dunst &

#dwm start loop
while true; do
    /usr/local/bin/dwm 2> /dev/null
done


