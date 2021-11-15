Config { 
font = "xft:UbuntuMono Nerd Font:style=Regular:pixelsize=13:antialias=true:hinting=true",additionalFonts = [ "xft:FontAwesome:pixelsize=13" ]
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , alpha = 221
       , position = TopW L 100 27
       , allDesktops = True
       , overrideRedirect = True
       , iconRoot = ".xmonad/xpm/"  -- default: "."
       , commands = [
              Run UnsafeStdinReader
            , Run Uptime ["-t","<icon=clock.xpm/> <days>:<hours>"] 6000
		    , Run Com ".xmonad/scripts/gmail" [] "count" 16000
            , Run Com "sh" ["-c", "checkupdates | wc -l"] "checkupdates" 21            
            , Run DiskU [("/","<icon=hdd.xpm/> <free>")] [] 60
            , Run CoreTemp ["-t","<icon=temp.xpm/> <core0>C | <icon=temp.xpm/> <core1>C","-L", "40", "-H", "60","-l", "#b3afc2", "-n", "#b3afc2", "-h", "red"] 50     
            , Run Cpu ["-t","<icon=cpu.xpm/> <total>%","-H","50","--normal","green","--high","red"] 20
            , Run Memory ["-t","<icon=ram.xpm/> <usedratio>%"] 10
            , Run Swap ["-t","<icon=swap.xpm/> <usedratio>%"] 10
            , Run Network "enp2s0" ["--template", "<icon=netup.xpm/> <tx>kB | <icon=netdown.xpm/> <rx>kB", "-L","1000","-H","5000", "--low", "gray", "--normal","green","--high","red"] 10
  		    , Run Alsa "default" "Master" ["-t", "<icon=volume.xpm/> <volume>%"] 
            , Run Date "<icon=calendar.xpm/> <fc=#b3afc2>%H:%M</fc>" "date" 10     
            , Run Weather "LGTS" ["-t","<station>: <tempC>C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000        
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<action=`xdotool key control+alt+g`><icon=haskell.xpm/></action><fc=#b3afc2> | </fc>%UnsafeStdinReader%} { %uptime% | %enp2s0% | %disku% | %coretemp% | %cpu% | %memory% | %swap% | %alsa:default:Master% | <icon=mail.xpm/> %count% | <icon=pacman_alt.xpm/> %checkupdates% | %date% |"}





