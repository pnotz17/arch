#!/bin/sh

DMENU="dmenu -i"

WALLPAPER_DIR="/data/Image"
WALLPAPER=`ls $WALLPAPER_DIR \
	| $DMENU -p "Choose a Wallpaper :"`

FEH="feh --bg-fill"

$FEH $WALLPAPER_DIR/$WALLPAPER	
