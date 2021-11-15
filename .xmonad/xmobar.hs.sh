Config { 
	font = "xft:UbuntuMono Nerd Font:style=Regular:pixelsize=13:antialias=true:hinting=true", additionalFonts = [ "xft:FontAwesome:pixelsize=11" ]
	bgColor = "#000000"
		, fgColor = "#b3afc2"     
		, alpha = 250
		, position = TopW L 100
		, lowerOnStart = True
		, hideOnStart = False
		, allDesktops = True
		, persistent = True
		, iconRoot = ".xmonad/xpm/"  -- default: "."
		
		, commands = [ 
		  Run UnsafeStdinReader
		, Run Com ".config/xmobar/scripts/gmail" [] "count" 16000
		, Run Com ".config/xmobar/scripts/pacman" [] "pacupdate" 16000
		, Run Com ".config/xmobar/scripts/unitemp" [] "temp" 2
		, Run Com "bash" [".config/xmobar/scripts/cpu"] "cpu" 20
		, Run Com "bash" [".config/xmobar/scripts/ram"] "ram" 20
		, Run Com ".config/xmobar/scripts/vol" [] "vol" 2
		, Run Com ".config/xmobar/scripts/netup" [] "up" 20
		, Run Com ".config/xmobar/scripts/netdown" [] "down" 20
		, Run Com ".config/xmobar/scripts/time" [] "date" 50
		]
		
, sepChar = "%"
, alignSep = "}{"
, template = "<action=`xdotool key control+alt+g`><icon=haskell_20.xpm/></action><fc=#b3afc2>|</fc>%UnsafeStdinReader%}{<fc=#b3afc2> | </fc><fc=#b3afc2> PACMAN: %pacupdate% </fc><fc=#b3afc2> | </fc><fc=#b3afc2> MAIL: %count% </fc><fc=#b3afc2> | </fc><fc=#b3afc2> CPU: %cpu% </fc><fc=#b3afc2> | </fc><fc=#b3afc2> RAM: %ram% </fc><fc=#b3afc2> | </fc><fc=#b3afc2> VOL: %vol% </fc><fc=#b3afc2> | </fc><fc=#b3afc2> UP: %up% </fc><fc=#b3afc2> | </fc><fc=#b3afc2> DOWN: %down% </fc><fc=#b3afc2> | </fc><fc=#b3afc2> %date% </fc><fc=#b3afc2> | </fc>"
}



