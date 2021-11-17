Config { 
font = "xft:UbuntuMono Nerd Font:style=Regular:pixelsize=13:antialias=true:hinting=true",additionalFonts = [ "xft:FontAwesome:pixelsize=13" ]
        bgColor = "#080808"
      , fgColor = "#b3afc2"     
      , alpha = 200
      , position = TopW L 100 24
      , lowerOnStart = True
      , hideOnStart = False
      , allDesktops = True
      , persistent = True
      , commands = [ 
			  Run UnsafeStdinReader
            , Run Com "/home/pnotz17/.xmonad/scripts/mail" [] "mail" 2
		    , Run Com "/home/pnotz17/.xmonad/scripts/pacman" [] "pacman" 2
		    , Run Com "/home/pnotz17/.xmonad/scripts/unitemp" [] "temp" 2
			, Run Com "/home/pnotz17/.xmonad/scripts/cpu" [] "cpu" 2
			, Run Com "/home/pnotz17/.xmonad/scripts/ram" [] "ram" 2
			, Run Com "/home/pnotz17/.xmonad/scripts/vol" [] "vol" 2
			, Run Com "/home/pnotz17/.xmonad/scripts/netup" [] "up" 2																
			, Run Com "/home/pnotz17/.xmonad/scripts/netdown" [] "down" 2
			, Run Com "/home/pnotz17/.xmonad/scripts/time" [] "time" 2
					]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " | %UnsafeStdinReader% } { <fc=#b3afc2> | </fc><fc=#b3afc2>   %pacman% </fc><fc=#b3afc2> | </fc><fc=#b3afc2>   %mail% </fc><fc=#b3afc2> | </fc><fc=#b3afc2>   %cpu% </fc><fc=#b3afc2> | </fc><fc=#b3afc2>   %ram% </fc><fc=#b3afc2> | </fc><fc=#b3afc2>   %vol% </fc><fc=#b3afc2> | </fc><fc=#b3afc2>   %up% </fc><fc=#b3afc2> | </fc><fc=#b3afc2>   %down% </fc><fc=#b3afc2> | </fc><fc=#b3afc2>   %time% </fc><fc=#b3afc2> | </fc>"}



