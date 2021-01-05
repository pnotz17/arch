-- xmonad config used by Vic Fryzel
import System.IO
import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.Renamed(renamed,Rename(Replace,CutWordsLeft))
import XMonad.Layout.ResizableTile
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Default Mod Key
myModMask = mod4Mask
-- Terminal
myTerminal = "st"
-- Location of your xmobarrc
myXmobarrc = "/home/panos21/.config/xmobar/.xmobarrc"
-- Width of the window border in pixels.
myBorderWidth = 1
-- Colors and borders
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#7c7c7c"--"#ffb6b0"

-- Workspaces
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape)
 -- $ [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
    $ ["1: dev","2: web","3: code","4: doc","5: mail","6: media","7: down","8: etc","9: misc"] 
  where
    clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

-- Layouts
myLayout = avoidStruts $ mySpacing (
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    ThreeColMid 1 (3/100) (1/2) |||
    tabbed shrinkText tabConfig |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)

  where
mySpacing = spacing 1

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}
   -- Window rules
myManageHook = composeAll
    [ className =? "Chromium"       --> doShift "2:web"
    , resource  =? "desktop_window" --> doIgnore
    , className =? "Gimp"           --> doFloat
    , className =? "mpv"            --> doFloat
    , isFullscreen --> (doF W.focusDown <+> doFullFloat)]

-- Startup hook
myStartupHook = return ()

-- Main
main = do
  xmproc <- spawnPipe ("xmobar " ++ myXmobarrc)
  xmonad $ defaults {
  logHook = dynamicLogWithPP $ xmobarPP
    { ppOutput = hPutStrLn xmproc
    , ppCurrent = xmobarColor "#83a598" "" . wrap "[" "]"   -- Visible but not current workspace
    , ppTitle = xmobarColor "#b3afc2" "" . shorten 50       -- Title of active window in xmobar
    }
    , manageHook = manageDocks <+> myManageHook
--  , startupHook = docksStartupHook <+> setWMName "LG3D"
    , startupHook = setWMName "LG3D"
    , handleEventHook = docksEventHook
  }

defaults = defaultConfig {
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    layoutHook         = smartBorders $ renamed [CutWordsLeft 1] $ myLayout,
    manageHook         = myManageHook,
    startupHook        = myStartupHook
}

-- Key bindings
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
 
  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)
     
  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))
  
  -- Toggle the status bar gap
  , ((modMask, xK_b),
     sendMessage ToggleStruts)    

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)
     
  -- Screenshot
  ,((modMask .|. shiftMask, xK_Print),
     spawn "scrot -e 'mv $f ~/Pictures/Screenshots/%Y-%m-%d-%H-%M-%S.png 2>/dev/null'")
  
  -- launch dmenu
   , ((modMask, xK_p),
    spawn "dmenu_run")

  -- Mute volume.
  ,((modMask .|. shiftMask, xK_m),
     spawn "amixer -q set Master toggle")

  -- Decrease volume.
  ,((modMask .|. shiftMask, xK_d),
     spawn "amixer -q set Master 5%-")

  -- Increase volume.
  ,((modMask .|. shiftMask, xK_i),
     spawn "amixer -q set Master 5%+")

  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Mouse bindings
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))
  ]
