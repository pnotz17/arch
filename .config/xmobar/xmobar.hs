Config { 
	font = "xft:UbuntuMono Nerd Font:style=Regular:pixelsize=13:antialias=true:hinting=true", 
	additionalFonts = [ "xft:FontAwesome:pixelsize=11" ]
       , borderColor = "black"
       , border = TopB
       , bgColor = "black"
       , fgColor = "grey"
       , alpha = 255
       , position = TopW L 100
       , allDesktops = True
       , overrideRedirect = True
       , iconRoot = ".xmonad/xpm/"  -- default: "."
       , commands = [
           -- Run Network "enp2s0" ["-L","0","-H","32", "--normal","green","--high","red"] 10
		      Run Network "enp2s0" ["--template", "Net: <tx>kB|<rx>kB", "-L","1000","-H","5000", "--low", "gray", "--normal","green","--high","red"] 10
            , Run Cpu ["-L","3","-H","50", "--normal","green","--high","red"] 10
            , Run Memory ["-t","Mem: <usedratio>%"] 10
            , Run Swap [] 10
		    , Run Date "%a %Y-%m-%d %H:%M:%S" "date" 10
		    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<action=`xdotool key control+alt+g`><icon=haskell_20.xpm/></action><fc=#b3afc2>|</fc>%UnsafeStdinReader%}{ %cpu% | %memory% | %swap% | %enp2s0% | <fc=#ee9a00>%date%</fc> |" }



