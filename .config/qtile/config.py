# Imports
from libqtile.config import Click, Drag, Group, Key, Screen
from libqtile.utils import guess_terminal
from libqtile import bar, layout, widget
from typing import List  # noqa: F401
from libqtile.lazy import lazy
from libqtile import hook
import subprocess
import os

# Variables 
mod = "mod4"                        

# Colors 
grey ="#808080"
tcsb ="#262626"
barc ="#111111"

keys = [
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod, "shift"], "h", lazy.layout.swap_left()),
    Key([mod, "shift"], "l", lazy.layout.swap_right()),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod, "shift"], "space", lazy.layout.flip()),
    Key([mod, "shift"], "Return", lazy.spawn("st")),
    Key([mod, "shift"], "b", lazy.spawn("qutebrowser")),
    Key([mod, "shift"], "f", lazy.spawn("pcmanfm")),
    Key([mod, "shift"], "c", lazy.window.kill()),
    Key([mod], "i", lazy.layout.grow()),
    Key([mod], "d", lazy.layout.shrink()),
    Key([mod], "n", lazy.layout.reset()),
    Key([mod], "o", lazy.layout.maximize()),
    Key([mod], "f", lazy.window.toggle_fullscreen()),
    Key([mod], "p", lazy.spawn("dmenu_run")),
    Key([mod], "Tab", lazy.next_layout()),
    Key([mod], "space", lazy.window.toggle_floating()),
    Key([mod, "control"], "r", lazy.restart()),
    Key([], "F12", lazy.spawn("amixer set Master Front 3+")),
    Key([], "F11", lazy.spawn("amixer set Master Front 3-")),
    Key([], "Print", lazy.spawn("scrot -e 'mv $f ~/Pictures/Screenshots/%Y-%m-%d-%H-%M-%S.png 2>/dev/null")),
]

group_names = [("dev", {'layout': 'monadtall'}),
			   ("web", {'layout': 'monadtall'}),
			   ("code", {'layout': 'monadtall'}),
			   ("doc", {'layout': 'monadtall'}),
			   ("mail", {'layout': 'monadtall'}),
			   ("media", {'layout': 'monadtall'}),
			   ("down", {'layout': 'monadtall'}),
			   ("etc", {'layout': 'monadtall'}),
			   ("misc", {'layout': 'monadtall'}),]

groups = [Group(name, **kwargs) for name, kwargs in group_names]

for i, (name, kwargs) in enumerate(group_names, 1):
    keys.append(Key([mod], str(i), lazy.group[name].toscreen()))        # Switch to another group
    keys.append(Key([mod, "shift"], str(i), lazy.window.togroup(name))) # Send current window to another group

def init_layout_theme():
    return {"margin":1,
            "border_width":1,
            "border_focus": "#807F94",
            "border_normal": "#807F94"
            }

layout_theme = init_layout_theme()

layouts = [
    layout.MonadTall(**layout_theme),
    layout.MonadWide(**layout_theme),
    layout.Matrix(**layout_theme),
    layout.Bsp(**layout_theme),
    layout.Floating(**layout_theme),
    layout.RatioTile(**layout_theme),
    layout.Max(**layout_theme)
]

widget_defaults = dict(
    font='Ubuntu Mono',
    fontsize=14,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [    
               widget.Image(
               filename = "~/.config/qtile/3",),
                  
               widget.Sep(
               linewidth = 1,
               padding = 12,),
               
               widget.GroupBox(
               rounded=False,
               spacing=10, 
               padding=0,
               this_current_screen_border=tcsb,
               highlight_method = "block",
               active = "#ffffff",
               inactive = grey,),   
               
               widget.Sep(
               linewidth = 1,
               padding = 12,),
               
               widget.CurrentLayout(
               foreground =grey,),
               
               widget.Sep(linewidth = 1,
               padding = 12,),
               
               widget.WindowName(
               foreground = grey,
               margin_x=6,), 
               
               widget.Spacer(),
            
               widget.CheckUpdates(
               distro='Arch_checkupdates',
               display_format=' Updates: {updates}',
               colour_have_updates="#FF0000",
               execute = '/home/panos21/.local/bin/pacupdate', 
               no_update_string = '',
               update_interval=60),
               
               # widget.Sep(
               # linewidth = 1,
               # padding = 12,),
               
               # widget.ThermalSensor(
               # foreground =grey,
               # fmt ='temp {}',),
               
               widget.Sep(
               linewidth = 1,
               padding = 12,),
              
               widget.CPU(
               format = 'cpu: {load_percent}%',
               foreground = grey,),
               
               widget.Sep(
               linewidth = 1,
               padding = 12,),
               
			   widget.Memory(
               foreground = grey,
               format = "mem: " '{MemUsed}M',
               update_interval = 1,),
               
               widget.Sep(
               linewidth = 1,
               padding = 12,),
               
               widget.Volume(
               foreground =grey,
               fmt ='vol: {}',),
               
               widget.Sep(
               linewidth = 1,
               padding = 12,),
               
               widget.Clock(
               foreground = grey,
               format = "%A, %B %d %H:%M",),
                     
               widget.Sep(
               linewidth = 1,
               padding = 12,),
               
               widget.Systray(
               padding = 5,),

            ],
            18,
            background=barc,opacity=0.99
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),start=lazy.window.get_size()),
    Click([mod],"Button2", lazy.window.bring_to_front())
]

follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
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
    {'wmclass': 'ssh-askpass'},
],  **layout_theme) #fullscreen_border_width = 0, border_width = 0)

auto_fullscreen = True
focus_on_window_activation = "smart"

wmname = "LG3D"
