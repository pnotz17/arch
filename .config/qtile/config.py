import os, subprocess
from libqtile.lazy import lazy
from libqtile import hook, bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Screen

mod  = 'mod4'
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
		lazy.spawn("scrot media/screenshots/%b%d::%H%M%S.png")),
]

mouse = [
	Drag([mod],"Button1",lazy.window.set_position_floating(),
		start=lazy.window.get_position()),
	Drag([mod],"Button3",lazy.window.set_size_floating(),
		start=lazy.window.get_size()),
	Click([mod],"Button2",lazy.window.bring_to_front())
]

groups= [
	Group("1",
		  label="dev",
		  ),
	
	Group("2",
		  label="www",
		  ),
	
	Group("3",
		  label="code",
		  ),
	
	Group("4",
		  label="sys",
		  ),
	
	Group("5",
		  label="doc"),
]

for i in range(len(groups)):
   keys.append(Key([mod], str((i)), lazy.group[str(i)].toscreen()))
   keys.append(Key([mod, "shift"], str((i)), lazy.window.togroup(str(i), switch_group=True)))

colors = {
    '0': '000000',
    '1': 'FFFFFF',
    '2': 'B5B5B5',
    '3': 'FF0000',
    '4': '121212',
    '5': 'B3AFC2',
    '6': 'B3AFC2'}

def init_layout_theme():
	return {"margin":1,
	"border_width"  :1,
	"border_focus"  : colors['5'],
	"border_normal" : colors['6']
	}
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
					  padding_left=6,padding_x=6, 
					  padding_y=2, 
					  vspace=2),
	layout.Max     	 (**layout_theme),
	layout.Floating	 (**layout_theme),
]

widget_defaults = dict(
	font     ='Droid Sans Mono Nerd Font',
	fontsize =13,
	padding  =4.75,
	)
extension_defaults = widget_defaults.copy()

screens = [
	Screen(
		top=bar.Bar( 
				[	
				widget.Image(
				filename="~/.config/qtile/images/1",
				),
				
				widget.TextBox(
				fmt='|',
				foreground=colors['2'],
				),
				
				widget.GroupBox(
				highlight_method="block",
				this_current_screen_border=colors['4'],
				active=colors['3'],
				inactive=colors['2'],
				),
				
				widget.TextBox(
				fmt='|',
				foreground=colors['2'],
				),
				
				widget.CurrentLayout(
				font='UbuntuMono Nerd Font',
                foreground=colors['1'],
                ),
	
				widget.TextBox(
				fmt='|',
				foreground=colors['2'],
				),
				
				widget.Spacer(),
			    
			  	widget.CheckUpdates(
				distro='Arch_checkupdates',
				display_format='  {updates}',
				colour_have_updates=colors['3'],
				execute='~/.local/bin/modules/pacupdate', 
				update_interval=60,
				),
				
				widget.TextBox(
				fmt=' | ',
				foreground=colors['2'],
				),
				
				widget.GenPollText(
				func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_cpu")).decode("utf-8").replace('\n', ''),
				update_interval=1, 
				foreground=colors['2'],
				),
				
				widget.TextBox(
				fmt=' | ',
				foreground=colors['2'],
				),
				
				widget.GenPollText(
				func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_ram")).decode("utf-8").replace('\n', ''),
				update_interval=1, 
				foreground=colors['2'],
				),
				
				widget.TextBox(
				fmt=' | ',
				foreground=colors['2'],
				),
				
				widget.GenPollText(
				func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_vol")).decode("utf-8").replace('\n', ''),
				update_interval=1, 
				foreground=colors['2'],
				),
				
				widget.TextBox(
				fmt=' | ',
				foreground=colors['2'],
				),
				
				widget.GenPollText(
				func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_up")).decode("utf-8").replace('\n', ''),
				update_interval=1, 
				foreground=colors['2'],
				),
				
				widget.TextBox(
				fmt=' | ',
				foreground=colors['2'],
				),
				
				widget.GenPollText(
				func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_do")).decode("utf-8").replace('\n', ''),
				update_interval=1, 
				foreground=colors['2'],
				),
				
				widget.TextBox(
				fmt=' | ',
				foreground=colors['2'],
				),
				
				widget.GenPollText(
				func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_time")).decode("utf-8").replace('\n', ''),
				update_interval=1, 
				foreground=colors['2'],
				),
				
				widget.TextBox(
				fmt=' | ',
				foreground=colors['2'],
				),
				
				widget.Systray(
				padding=5,
				),
				],
			20,
		background=colors['0'],
	opacity=0.90),),
]

dgroups_key_binder = None
dgroups_app_rules = []

main = None
@hook.subscribe.startup_once
def start_once():
    home = os.path.expanduser('~')
    subprocess.call([home + '/.config/qtile/autostart.sh'])

@hook.subscribe.startup
def start_always():
    subprocess.Popen(['xsetroot','-cursor_name','left_ptr'])

@hook.subscribe.client_new
def set_floating(window):
    if (window.window.get_wm_transient_for()
     or window.window.get_wm_type() in floating_types):
     window.floating=True

floating_types=["notification","toolbar","splash","dialog"]
follow_mouse_focus=True
bring_front_click=False
cursor_warp=False
floating_layout=layout.Floating(float_rules=[
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},
    {'wmclass': 'makebranch'},
    {'wmclass': 'maketag'},
    {'wmclass': 'Create New File'},
    {'wmclass': 'Pinentry-gtk-2'},
    {'wmclass': 'transmission-gtk'},
    {'wname'  : 'branchdialog'}, 
    {'wname'  : 'Confirm Delete'},  
   	{'wname'  : 'Open File'},
   	{'wname'  : 'Move Folder'},
   	{'wname'  : 'Rename Required'},  
],**layout_theme)
auto_fullscreen=True
focus_on_window_activation="smart"

wmname="LG3D"

