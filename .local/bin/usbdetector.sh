#!/bin/bash

if [[ "$(dmesg -S | tail | grep "usb-storage")" =~ "USB Mass Storage device detected" ]]; then
           icon=" ��️ "
           else
           icon=""
   fi
   
   printf "%s%s\\n" "$icon"
