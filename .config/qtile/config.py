import os, subprocess
from libqtile.lazy import lazy
from libqtile import hook, bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Screen

RED      ="#FF0000"
GREY     ="#B5B5B5"
TCSB     ="#333333"
WHITE    ="#FFFFFF"
BLACK    ="#000000"
NBORDER  ="#B3AFC2"
FBORDER  ="#B3AFC2"
BARCOLOR ="#000000"

mod                        ="mod4"                        
follow_mouse_focus         =True
auto_fullscreen            =True
bring_front_click          =False
cursor_warp                =False
focus_on_window_activation ="smart"
wmname                     ="LG3D" 

keys = [
	Key(
		[mod, "control"], "r", 
		lazy.restart()),
	Key(
		[mod, "shift"], "Return", 
		lazy.spawn("st")),
	Key(
		[mod, "shift"], "c", 
		lazy.window.kill()),
	Key(
		[mod], "space", 
		lazy.next_layout()),	
	Key(
		[mod], "p", 
		lazy.spawn("dmenu_run")),
	Key(
		[mod], "t", 
		lazy.window.toggle_floating()),
	Key(
		[mod], "f", 
		lazy.window.toggle_fullscreen()),
	Key(
		[mod], "h", 
		lazy.layout.left()),
	Key(
		[mod], "l", 
		lazy.layout.right()),
	Key(
		[mod], "j", 
		lazy.layout.down()),
	Key(
		[mod], "k", 
		lazy.layout.up()),
	Key(
		[mod, "shift"], "h", 
		lazy.layout.swap_left()),
	Key(
		[mod, "shift"], "l", 
		lazy.layout.swap_right()),
	Key(
		[mod, "shift"], "j", 
		lazy.layout.shuffle_down()),
	Key(
		[mod, "shift"], "k", 
		lazy.layout.shuffle_up()),
	Key(
		[mod], "Return", 
		lazy.layout.swap_main()),
	Key(
		[mod, "control"], 'h', 
		lazy.layout.shrink()),
	Key(
		[mod, "control"],  'l', 
		lazy.layout.grow()),
	Key(
		[mod, "control"],  'n', 
		lazy.layout.reset()),
	Key(
		[mod, "control"], 'm', 
		lazy.layout.maximize()),
	Key(
		[mod, "shift"], "b", 
		lazy.spawn("firefox")),
	Key(
		[mod, "shift"], "f", 
		lazy.spawn("spacefm")),
	Key(
		[mod, "shift"], "g", 
		lazy.spawn("geany")),
	Key(
		[mod, "shift"], "m", 
		lazy.spawn("st -e mutt")),
	Key(
		[mod], "d", 
		lazy.spawn(".local/bin/dm_ytdl")),
	Key(
		[mod, "control"], "d", 
		lazy.spawn(".local/bin/dm_fm")),	
	Key(
		[mod, "control"], "e", 
		lazy.spawn(".local/bin/dm_ed")),
	Key(
		[mod, "control"], "q", 
		lazy.spawn(".local/bin/dm_power")),
	Key(
		[], "F11", 
		lazy.spawn("amixer set Master Front 2-")),
	Key(
		[], "F12", 
		lazy.spawn("amixer set Master Front 2+")),
	Key(
		[], "Print", 
		lazy.spawn("scrot media/screenshots/%b%d::%H%M%S.png")),]

mouse = [
	Drag([mod],"Button1",lazy.window.set_position_floating(),
		start=lazy.window.get_position()),
	Drag([mod],"Button3",lazy.window.set_size_floating(),
		start=lazy.window.get_size()),
	Click([mod],"Button2",lazy.window.bring_to_front())]

group_names = [
	("dev", {'layout': 'MonadTall'}),
	("www", {'layout': 'MonadTall'}),
	("code",{'layout': 'MonadTall'}),
	("sys", {'layout': 'MonadTall'}),
	("doc", {'layout': 'MonadTall'}),]
groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i,(name, kwargs)in enumerate(group_names,1):
    keys.append(Key([mod],str(i),lazy.group[name].toscreen()))               
    keys.append(Key([mod,"shift"],str(i),lazy.window.togroup(name))) 
		
def init_layout_theme():
	return {"margin":1,
	"border_width"  :1,
	"border_focus"  : FBORDER,
	"border_normal" : NBORDER}
layout_theme = init_layout_theme()

layouts = [
	layout.MonadTall (**layout_theme),
	layout.MonadWide (**layout_theme),
	layout.Tile		 (**layout_theme),
	layout.RatioTile (**layout_theme),
	layout.Matrix    (**layout_theme),
	layout.TreeTab   (font="Mono", 
					  fontsize=13, 
					  panel_width=175, 
					  bg_color="#1c1b1c", 
					  active_bg="#606060", 
					  inactive_bg="#404040", 
					  border_width=2, 
					  padding_left=6, padding_x=6, 
					  padding_y=2, 
					  vspace=2),
	layout.Max     	 (**layout_theme),
	layout.Floating	 (**layout_theme),]

widget_defaults = dict(
	font     ='DaddyTimeMono Nerd Font',
	fontsize =13,
	padding  =4.75,)
extension_defaults = widget_defaults.copy()

screens = [
	Screen(
		top=bar.Bar( 
				[	
				widget.Image(
				filename="~/.config/qtile/images/1",),
				
				widget.TextBox(
				fmt='|',
				foreground=GREY,),
				
				widget.GroupBox(
				this_current_screen_border=TCSB,
				highlight_method="block",
				active=WHITE,
				inactive=GREY,),
				
				widget.TextBox(
				fmt='|',
				foreground=GREY,),
				
				widget.CurrentLayout(
                font="UbuntuMono Nerd Font",				
                foreground=WHITE,),
	
				widget.TextBox(
				fmt='|',
				foreground=GREY,),
				
				widget.Spacer(),
			    
			  	widget.CheckUpdates(
				distro='Arch_checkupdates',
				display_format=' {updates}',
				colour_have_updates=RED,
				execute='~/.local/bin/modules/pacupdate', 
				update_interval=60),
				
				widget.TextBox(
				fmt=' | ',
				foreground=GREY,),
				
				widget.GenPollText(
				func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_cpu")).decode("utf-8").replace('\n', ''),
				update_interval=1, 
				foreground=GREY,),
				
				widget.TextBox(
				fmt=' | ',
				foreground=GREY,),
				
				widget.GenPollText(
				func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_ram")).decode("utf-8").replace('\n', ''),
				update_interval=1, 
				foreground=GREY,),
				
				widget.TextBox(
				fmt=' | ',
				foreground=GREY,),
				
				widget.GenPollText(
				func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_vol")).decode("utf-8").replace('\n', ''),
				update_interval=1, 
				foreground=GREY,),
				
				widget.TextBox(
				fmt=' | ',
				foreground=GREY,),
				
				widget.GenPollText(
				func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_up")).decode("utf-8").replace('\n', ''),
				update_interval=1, 
				foreground=GREY,),
				
				widget.TextBox(
				fmt=' | ',
				foreground=GREY,),
				
				widget.GenPollText(
				func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_do")).decode("utf-8").replace('\n', ''),
				update_interval=1, 
				foreground=GREY,),
				
				widget.TextBox(
				fmt=' | ',
				foreground=GREY,),
				
				widget.GenPollText(
				func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_time")).decode("utf-8").replace('\n', ''),
				update_interval=1, 
				foreground=GREY,),
				
				widget.TextBox(
				fmt=' | ',
				foreground=GREY,),
				
				widget.Systray(
				padding=5,),
				],
			20,
		background=BARCOLOR,
	opacity=0.90),),
]

floating_layout = layout.Floating(float_rules=[
	{'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},
    {'wmclass': 'Pinentry-gtk-2'},
    {'wname'  : 'Open File'},
*layout.Floating.default_float_rules],**layout_theme)

