# Imports
import os
import subprocess
from libqtile import hook
from libqtile.lazy import lazy
from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Screen

# Colors 
GREY     ="#808080"
TCSB     ="#262626"
WHITE    ="#FFFFFF"
BARCOLOR ="#000000"

# Misc settings
mod                        = "mod4"                        
follow_mouse_focus         = True
bring_front_click          = False
cursor_warp                = False
auto_fullscreen            = True
focus_on_window_activation = "smart"
wmname                     = "LG3D" 

# Key Bindings
keys = [
	
	# Window manager controls
	Key([mod, "control"], "r", 
		lazy.restart()),
	Key([mod, "shift"], "Return", 
		lazy.spawn("st")),
	Key([mod, "shift"], "space", 
		lazy.next_layout()),
	Key([mod, "shift"], "c", 
		lazy.window.kill()),
	Key([mod], "p", 
		lazy.spawn("dmenu_run")),
	Key([mod], "t", 
		lazy.window.toggle_floating()),
	Key([mod], "f", 
		lazy.window.toggle_fullscreen()),
	
	# Move focus
	Key([mod], "h", 
		lazy.layout.left()),
	Key([mod], "l", 
		lazy.layout.right()),
	Key([mod], "j", 
		lazy.layout.down()),
	Key([mod], "k", 
		lazy.layout.up()),
	
	# Move window
	Key([mod, "shift"], "h", 
		lazy.layout.swap_left()),
	Key([mod, "shift"], "l", 
		lazy.layout.swap_right()),
	Key([mod, "shift"], "j", 
		lazy.layout.shuffle_down()),
	Key([mod, "shift"], "k", 
		lazy.layout.shuffle_up()),
	Key([mod], "Return", 
		lazy.layout.swap_main()),
	
	# Alter window size
	Key([mod, "shift"], 'h', 
		lazy.layout.shrink()),
	Key([mod, "shift"],  'l', 
		lazy.layout.grow()),
	Key([mod, "shift"],  'n', 
		lazy.layout.reset()),
	Key([mod, "shift"], 'm', 
		lazy.layout.maximize()),
	  
	# TreeTab controls          
	Key([mod, "control"], "k",
		lazy.layout.section_up()),          
	Key([mod, "control"], "j", 
		lazy.layout.section_down()),
	
	# Extras 
	Key([mod, "shift"], "b", 
		lazy.spawn("firefox")),
	Key([mod, "shift"], "f", 
		lazy.spawn("spacefm")),
	Key([mod, "shift"], "g", 
		lazy.spawn("geany")),
	Key([mod, "shift"], "m", 
		lazy.spawn("st -e mutt")),
	Key([mod, "control"], "d", 
		lazy.spawn(".local/bin/dm_fm")),
	Key([mod, "control"], "e", 
		lazy.spawn(".local/bin/dm_ed")),
	Key([mod, "control"], "q", 
		lazy.spawn(".local/bin/dm_power")),
	Key([], "F11", 
		lazy.spawn("amixer set Master Front 2-")),
	Key([], "F12", 
		lazy.spawn("amixer set Master Front 2+")),
	Key([], "Print", 
		lazy.spawn("scrot media/screenshots/%b%d::%H%M%S.png")),]

# Mouse bindings
mouse = [
	Drag([mod], "Button1", 
		lazy.window.set_position_floating(),start=lazy.window.get_position()),
	Drag([mod], "Button3", 
		lazy.window.set_size_floating(),start=lazy.window.get_size()),
	Click([mod],"Button2", 
		lazy.window.bring_to_front())]

# Workspaces
group_names = [
	("dev", {'layout': 'Tile'}),
	("www", {'layout': 'Tile'}),
	("code",{'layout': 'Tile'}),
	("sys", {'layout': 'Tile'}),
	("doc", {'layout': 'Tile'}),]
groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([mod], str(i), 
		lazy.group[name].toscreen()))               
    keys.append(Key([mod, "shift"], str(i), 
		lazy.window.togroup(name))) 				

# Layout defaults
def init_layout_theme():
	return {"margin":1,
	"border_width"  :1,
	"border_focus"  : "#b3afc2",
	"border_normal" : "#b3afc2"}
layout_theme = init_layout_theme()

# Layouts
layouts = [
	layout.Tile(shift_windows=True, **layout_theme),
	layout.MonadTall(**layout_theme),
	layout.MonadWide(**layout_theme),
	layout.TreeTab(          
	font             = "FreeMono Normal",          
	fontsize         = 13,          
	sections         = ["FIRST", "SECOND"],          
	section_fontsize = 13,          
	bg_color         = "141414",          
	active_bg        = "90C435",          
	active_fg        = "000000",          
	inactive_bg      = "384323",          
	inactive_fg      = "a0a0a0",          
	padding_y        = 5,          
	section_top      = 10,   
	panel_width      = 250),
	layout.Max     (),
	layout.Floating(),]

# Widget defaults
widget_defaults = dict(
	font     ='FreeMono Normal',
	fontsize =13,
	padding  =4.75,)
extension_defaults = widget_defaults.copy()

# Bar setup
screens = [Screen(top=bar.Bar(
[	widget.Image(
	filename = "~/.config/qtile/images/1",),
	
	widget.TextBox(
	fmt ='|',
	foreground = WHITE,),
	
	widget.GroupBox(
	this_current_screen_border=TCSB,
	highlight_method = "block",
	active = "#ffffff",
	inactive = GREY,),
	              
	widget.TextBox(
	fmt =' | ',
	foreground = WHITE,),

	widget.CurrentLayout(
	foreground =GREY,),
	
	widget.TextBox(
	fmt =' | ',
	foreground = WHITE,),

	widget.Spacer(),
	
	widget.CheckUpdates(
	update_interval = 1800,
	distro = "Arch_checkupdates",
    display_format='  {updates}',
	colour_have_updates="#FF0000",),

	widget.TextBox(
	fmt ='|',
	foreground = WHITE,),
	
	widget.GenPollText(
	func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_cpu")).decode("utf-8").replace('\n', ''),
	update_interval=1, 
	foreground=WHITE,),
	
	widget.TextBox(
	fmt =' | ',
	foreground = WHITE,),

	widget.GenPollText(
	func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_ram")).decode("utf-8").replace('\n', ''),
	update_interval=1, 
	foreground=WHITE,),
	
	widget.TextBox(
	fmt =' | ',
	foreground = WHITE,),
	
	widget.GenPollText(
	func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_vol")).decode("utf-8").replace('\n', ''),
	update_interval=1, 
	foreground=WHITE,),
	
	widget.TextBox(
	fmt =' | ',
	foreground = WHITE,),

	widget.GenPollText(
	func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_up")).decode("utf-8").replace('\n', ''),
	update_interval=1, 
	foreground=WHITE,),
	
	widget.TextBox(
	fmt =' | ',
	foreground = WHITE,),
	
    widget.GenPollText(
	func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_do")).decode("utf-8").replace('\n', ''),
	update_interval=1, 
	foreground=WHITE,),
	
	widget.TextBox(
	fmt =' | ',
	foreground = WHITE,),

	widget.GenPollText(
	func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/modules/sb_time")).decode("utf-8").replace('\n', ''),
	update_interval=1, 
	foreground=WHITE,),
	
	widget.TextBox(
	fmt =' | ',
	foreground = WHITE,),

	widget.Systray(
	padding = 5,),
]
,20,background=BARCOLOR,opacity=0.90),),]

# Floating windows
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
    {'wmclass': 'makebranch'},
    {'wmclass': 'maketag'},
    {'wmclass': 'ssh-askpass'},
    {'wname'  : 'branchdialog'},
    {'wname'  : 'Open File'},
    {'wname'  : 'pinentry'},
*layout.Floating.default_float_rules]
,**layout_theme)
