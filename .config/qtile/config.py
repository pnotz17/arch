# Imports
from libqtile.config import Click, Drag, Group, Key, Screen
from libqtile.utils import guess_terminal
from libqtile import bar, layout, widget
from typing import List  # noqa: F401
from libqtile.lazy import lazy
from libqtile import hook
import subprocess
import os

#  Variables 
wmname = "LG3D"
mod    = "mod4"                        
auto_fullscreen     = True
follow_mouse_focus  = True
bring_front_click   = False
cursor_warp         = False
focus_on_window_activation = "smart"

# Colors 
grey1 ="#808080"
grey2 ="#808080"
tcsb ="#262626"
barc ="#080808"

# Key bindings
keys = [
	# Window manager controls
	Key([mod, "control"], "r", lazy.restart()),
	Key([mod, "control"], "q", lazy.restart()),
	Key([mod, "shift"], "Return", lazy.spawn("st")),
	Key([mod, "shift"], "space", lazy.next_layout()),
	Key([mod, "shift"], "c", lazy.window.kill()),
	Key([mod], "p", lazy.spawn("dmenu_run")),
	Key([mod], "t", lazy.window.toggle_floating()),
	Key([mod], "f", lazy.window.toggle_fullscreen()),
	
	# Move Focus
	Key([mod], "h", lazy.layout.left()),
	Key([mod], "l", lazy.layout.right()),
	Key([mod], "j", lazy.layout.down()),
	Key([mod], "k", lazy.layout.up()),
	
	# Move Window
	Key([mod, "shift"], "h", lazy.layout.swap_left()),
	Key([mod, "shift"], "l", lazy.layout.swap_right()),
	Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
	Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
	Key([mod], "Return", lazy.layout.swap_main()),
	
	# Alter Window Size
	Key([mod, "shift"], 'h', lazy.layout.shrink()),
	Key([mod, "shift"],  'l', lazy.layout.grow()),
	Key([mod, "shift"],  'n', lazy.layout.reset()),
	Key([mod, "shift"], 'm', lazy.layout.maximize()),
	  
	# Treetab controls          
	Key([mod, "control"], "k",lazy.layout.section_up()),          
	Key([mod, "control"], "j", lazy.layout.section_down()),
	    
	# Extras
	Key([mod, "shift"], "b", lazy.spawn("qutebrowser")),
	Key([mod, "shift"], "f", lazy.spawn("pcmanfm")),
	Key([], "F12", lazy.spawn("amixer set Master Front 2+")),
	Key([], "F11", lazy.spawn("amixer set Master Front 2-")),
	Key([], "Print", lazy.spawn("scrot -e 'mv $f ~/Pictures/Screenshots/%Y-%m-%d-%H-%M-%S.png 2>/dev/null")),]

# Mouse bindings
mouse = [
	Drag([mod], "Button1", lazy.window.set_position_floating(),start=lazy.window.get_position()),
	Drag([mod], "Button3", lazy.window.set_size_floating(),start=lazy.window.get_size()),
	Click([mod],"Button2", lazy.window.bring_to_front())]

# Groups
group_names = [
	("dev", {'layout': 'Tile'}),
	("www", {'layout': 'Tile'}),
	("code", {'layout': 'Tile'}),
	("sys", {'layout': 'Tile'}),
	("doc", {'layout': 'Tile'}),]

groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))                # Switch to another group
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name))) # Send current window to another group

# Layout variables
def init_layout_theme():
	return {"margin":1,
	"border_width":0,
	"border_focus": "#ffffff",
	"border_normal": "#808080"}
	
layout_theme = init_layout_theme()

# Layouts
layouts = [
	layout.Tile(shift_windows=True, **layout_theme),
	layout.MonadTall(**layout_theme),
	layout.MonadWide(**layout_theme),
	layout.TreeTab(          
    font = "Ubuntu",          
    fontsize = 14,          
    sections = ["FIRST", "SECOND"],          
    section_fontsize = 15,          
    bg_color = "141414",          
    active_bg = "90C435",          
    active_fg = "000000",          
    inactive_bg = "384323",          
    inactive_fg = "a0a0a0",          
    padding_y = 5,          
    section_top = 10,   
    panel_width = 250),
    layout.Max(**layout_theme),
	layout.Floating(**layout_theme,)]
	
# Widget variables
widget_defaults = dict(
	font='Mononoki-Regular Nerd Font  ',
	fontsize=14,
	padding=4.75,)

extension_defaults = widget_defaults.copy()

# Screen variables
screens = [Screen(top=bar.Bar(
[    
	widget.Image(
	filename = "~/.config/qtile/images/1",),
	
	widget.TextBox(
	fmt ='',
	foreground = grey2,),
	
	widget.GroupBox(
	this_current_screen_border=tcsb,
	highlight_method = "block",
	active = "#ffffff",
	inactive = grey1,),
	              
	widget.TextBox(
	fmt ='',
	foreground = grey2,),

	widget.CurrentLayout(
	foreground =grey1,),
	
	widget.TextBox(
	fmt ='',
	foreground = grey2,),

	widget.Spacer(),
	
	widget.CheckUpdates(
	distro='Arch_checkupdates',
	display_format=' Updates: {updates}',
	colour_have_updates="#FF0000",
	execute = '/home/panos21/.local/bin/pacupdate', 
	no_update_string = '',
	update_interval=60),
	
	# widget.TextBox(
	# fmt ='',
	# foreground = grey2,),

	# widget.ThermalSensor(
	# foreground =grey1,
	# fmt ='tem:    {}',),
	
	widget.TextBox(
	fmt ='',
	foreground = grey2,),

	widget.CPU(
	format = 'cpu:   {load_percent}%',
	foreground = grey1,),
	
	widget.TextBox(
	fmt ='',
	foreground = grey2,),

	widget.Memory(
	foreground = grey1,
	format = "mem:   {MemUsed}M",
	update_interval = 1,),
	
	widget.TextBox(
	fmt ='',
	foreground = grey2,),

	widget.Volume(
	foreground =grey1,
	fmt ='vol:   {}',),
	
	widget.TextBox(
	fmt ='',
	foreground = grey2,),

	widget.Net(
	interface = "enp2s0",
	foreground =grey1,
	format = 'up:   {up}', ),
	
	widget.TextBox(
	fmt ='',
	foreground = grey2,),
	
	widget.Net(
	interface = "enp2s0",
	foreground =grey1,
	format = 'do:   {down}', ),
	
	widget.TextBox(
	fmt ='',
	foreground = grey2,),

	widget.Clock(
	foreground = grey1,
	format ="   %a, %b %d  %H:%M",),
	
	widget.TextBox(
	fmt ='',
	foreground = grey2,),

	widget.Systray(
	padding = 5,),
	
],20,background=barc,opacity=0.95),),
]
# Floating rules
floating_layout = layout.Floating(float_rules=[
	{'wmclass': 'Confirm"'},
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
	{'wmclass': 'mpv'},
	{'wname': 'branchdialog'},
	{'wname': 'Open File'},
	{'wname': 'pinentry'},
	{'wmclass': 'ssh-askpass'},],  **layout_theme) #fullscreen_border_width = 0, border_width = 0)

