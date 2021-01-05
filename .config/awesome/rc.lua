	-- Required libraries
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
require("awful.hotkeys_popup.keys")
require("awful.autofocus")
	-- Error handling
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
	-- Theme & Font
beautiful.init("/home/panos21/.config/awesome/custom/default/theme.lua") 
beautiful.font = "Ubuntu Mono 13"	
	-- Variable definitions
terminal = "st"
editor = "vim"
editor_cmd = terminal .. " -e " .. editor
	-- Default modkey
modkey = "Mod4"	
	-- Table of layouts 
awful.layout.layouts = {
       awful.layout.suit.tile,
       awful.layout.suit.tile.left,
       awful.layout.suit.tile.bottom,
       awful.layout.suit.tile.top,
       --awful.layout.suit.floating,
}
	-- Wallpaper
awful.screen.connect_for_each_screen(function(s)
    -- set_wallpaper(s)
    -- Gaps
beautiful.useless_gap = 1
	-- Tag table.
      awful.tag({ "➊", "➋", "➌", "➍", "➎", "➏", "➐", "➑", "➒" }, s, awful.layout.layouts[1])
	-- Menu
local menu = {}
	-- Create a laucher widget and a main menu
menu["awesome"] = {
 { "edit config", editor_cmd .. " " .. awesome.conffile },
}
	-- Applications menu list.
menu["apps"] = {
 { "File manager", "pcmanfm" },
 { "Terminal", "st" },
 { "Web browser", "firefox" },
 { "Editor", "geany" },
}
	-- Create a menu containing applications, settings, and scripts for the
	-- launcher.
local makeMenu = function()
  local items = {}
	-- Add all menus in order to the items table.
  items[#items + 1] = { "Apps", menu["apps"] }
  items[#items + 1] = { "Awesome", menu["awesome"] }
	-- Return the built menu.
  return awful.menu({
    items = items
  })
end
	-- Create the main menu.
mymainmenu = makeMenu()
mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })
    -- Wibar
mytextclock = wibox.widget.textclock()
	-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
awful.button({ }, 1, function(t) t:view_only() end),
awful.button({ modkey }, 1, function(t)
						  if client.focus then
							  client.focus:move_to_tag(t)
						  end
					  end),
awful.button({ }, 3, awful.tag.viewtoggle),
awful.button({ modkey }, 3, function(t)
						  if client.focus then
							  client.focus:toggle_tag(t)
						  end
					  end),
awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = gears.table.join(
awful.button({ }, 1, function (c)
					  if c == client.focus then
						  c.minimized = true
					  else
						  c:emit_signal(
							  "request::activate",
							  "tasklist",
							  {raise = true}
						  )
					  end
				  end),
awful.button({ }, 3, function()
					  awful.menu.client_list({ theme = { width = 250 } })
				  end),
awful.button({ }, 4, function ()
					  awful.client.focus.byidx(1)
				  end),
awful.button({ }, 5, function ()
					  awful.client.focus.byidx(-1)
				  end))
	-- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
         -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons
    }
	     -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }
		 -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })
		 -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        {-- Left widgets
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        {-- Right widgets
            layout = wibox.layout.fixed.horizontal,
         -- mykeyboardlayout,
            wibox.widget.systray(),
            mytextclock,
            s.mylayoutbox,
        },
    }
end)
	-- Mouse bindings
root.buttons(gears.table.join(
    -- awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
	-- Key bindings
globalkeys = gears.table.join(
    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,{description="show help", group="awesome"}),
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,{description = "view previous", group = "tag"}),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,{description = "view next", group = "tag"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,{description = "go back", group = "tag"}),
    awful.key({ modkey,           }, "j",function ()awful.client.focus.byidx( 1)end,{description = "focus next by index", group = "client"}),
    awful.key({ modkey,           }, "k",function ()awful.client.focus.byidx(-1)end,{description = "focus previous by index", group = "client"}),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end,{description = "show main menu", group = "awesome"}),
	-- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,{description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,{description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,{description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,{description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,{description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",function ()awful.client.focus.history.previous()if client.focus then client.focus:raise()end end,{description = "go back", group = "client"}),
	-- Standard program
    awful.key({ modkey, "Shift" }, "Return", function () awful.spawn(terminal) end,{description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,{description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,{description = "quit awesome", group = "awesome"}),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,{description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,{description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,{description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,{description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,{description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,{description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,{description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,{description = "select previous", group = "layout"}),
    awful.key({ modkey, "Control" }, "n",     function ()local c = awful.client.restore()
    if c then c:emit_signal("request::activate", "key.unminimize", {raise = true})end end,{description = "restore minimized", group = "client"}),
	-- My keybindings
    awful.key({ modkey, "Shift" }, "w", function () awful.util.spawn("firefox") end),
    awful.key({ modkey, "Shift" }, "f", function () awful.util.spawn("pcmanfm") end),
    -- Multimedia
    awful.key({ }, "F12", function ()awful.util.spawn("amixer set Master 9%+", false) end),
    awful.key({ }, "F11", function ()awful.util.spawn("amixer set Master 9%-", false) end),
    awful.key({ }, "F10", function ()awful.util.spawn("amixer set Master toggle", false) end),
    --Screenshot
    awful.key({ }, "Print", function () awful.util.spawn("scrot -e 'mv $f ~/Pictures/Screenshots/%Y-%m-%d-%H-%M-%S.png 2>/dev/null'", false) end),
	-- Show/Hide Wibox
    awful.key({ modkey }, "b", function ()for s in screen do s.mywibox.visible = not s.mywibox.visible
    if s.mybottomwibox then s.mybottomwibox.visible = not s.mybottomwibox.visible end end end,{description = "toggle wibox", group = "awesome"}),
    -- Prompt
    awful.key({ modkey }, "d", function () awful.util.spawn("dmenu_run") end,{description = "dmenu", group = "launcher"}),
	-- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end,{description = "show the menubar", group = "launcher"})
)

clientkeys = gears.table.join(
    awful.key({ modkey,           }, "f",function (c)c.fullscreen = not c.fullscreen c:raise()end,{description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",function (c) c:kill()                                end,{description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                      ,{description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end ,{description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end ,{description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end ,{description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",      function (c)c.minimized = true end ,{description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",function (c)c.maximized = not c.maximized c:raise()end  ,{description = "(un)maximize", group = "client"}),
    awful.key({ modkey, "Control" }, "m",function (c)c.maximized_vertical = not c.maximized_vertical c:raise()end ,{description = "(un)maximize vertically", group = "client"}),
    awful.key({ modkey, "Shift"   }, "m",function (c)c.maximized_horizontal = not c.maximized_horizontal c:raise()end ,{description = "(un)maximize horizontally", group = "client"})
)
	-- Bind all key numbers to tags.
	-- Be careful: we use keycodes to make it work on any keyboard layout.
	-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
    -- View tag only.
    awful.key({ modkey }, "#" .. i + 9,
    function ()local screen = awful.screen.focused()local tag = screen.tags[i]if tag then tag:view_only()end end,
                  {description = "view tag #"..i, group = "tag"}),
	-- Toggle tag display.
    awful.key({ modkey, "Control" }, "#" .. i + 9,function ()
    local screen = awful.screen.focused()local tag = screen.tags[i]if tag then awful.tag.viewtoggle(tag)end end,
                  {description = "toggle tag #" .. i, group = "tag"}),
    -- Move client to tag.
    awful.key({ modkey, "Shift" }, "#" .. i + 9,
    function ()if client.focus then local tag = client.focus.screen.tags[i]if tag then client.focus:move_to_tag(tag)end end end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
    -- Toggle tag on focused client.
    awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
    function ()if client.focus then local tag = client.focus.screen.tags[i]if tag then client.focus:toggle_tag(tag)end end end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)c:emit_signal("request::activate", "mouse_click", {raise = true})awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)c:emit_signal("request::activate", "mouse_click", {raise = true})awful.mouse.client.resize(c)
    end)
)
	-- Set keys
root.keys(globalkeys)
	-- Rules
awful.rules.rules = {
    -- All clients will match this rule.
     { rule = { },
       properties = { border_width = beautiful.border_width,
                      border_color = beautiful.border_normal,
                      focus = true,
                      keys = clientkeys,
                      buttons = clientbuttons,
	                  size_hints_honor = false,
	                  maximized_horizontal = false,
                      maximized_vertical = false,
                      maximized = false,
                     }
    }
}
	-- Signals
	-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)
		-- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
		-- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))
		-- The title goes in the middle
        local title = awful.titlebar.widget.titlewidget(c)
        title:buttons(awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                ))
		-- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(title)

        awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
