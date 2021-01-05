
import System.Exit
import Data.Maybe (Maybe, isNothing, fromJust)
import qualified Data.List as L
import qualified Data.Map as M
import GHC.IO.Handle

-- Xmonad Core
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Config.Desktop

-- Layouts
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ResizableTile
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.SimpleFloat
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.GridVariants

-- Actions
import XMonad.Actions.Navigation2D
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer
import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive

-- Utils
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Run
import XMonad.Util.EZConfig

--  Terminal of choice
myTerminal = "st"
-- Window focus follows mouse
myFocusFollowsMouse = True
-- Clicking on widow focuses and passes the click forward
myClickJustFocuses = False
-- Width of the window border in pixels.
myBorderWidth = 1
-- Set border color when unfocused
myNormalBorderColor  = "#474646"
-- Set norder color when focused
myFocusedBorderColor = "#83a598"
-- Defines with mod key to use (mod4mask == super)
myModMask = mod4Mask
-- EwmhDesktops users should change this to ewmhDesktopsEventHook
myEventHook = mempty
-- Startup hook, not doing anything
myStartupHook = do
  setWMName "LG3D"
  startupHook desktopConfig

-- Workspaces
xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape)
               -- $ [" 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 "]
               $ ["1: dev","2: web","3: code","4: doc","5: mail","6: media","7: down","7: etc","7: misc"] 

  where
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

-- Key bindings. Add, modify or remove key bindings here.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)    -- launch a terminal
    , ((modm,               xK_p     ), spawn "dmenu_run")    -- launch dmenu
    , ((modm .|. shiftMask, xK_f     ), spawn "pcmanfm")    -- launch pcmanfm
    , ((modm .|. shiftMask, xK_c     ), kill)    -- close focused window
    , ((modm,               xK_space ), sendMessage NextLayout)     -- Rotate through the available layout algorithms
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)    --  Reset the layouts on the current workspace to default
    , ((modm,               xK_n     ), refresh)    -- Resize viewed windows to the correct size
    , ((modm,               xK_f), sendMessage $ Toggle FULL)   -- Toggle current focus window to fullscreen
    , ((modm,               xK_Tab   ), windows W.focusDown)    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)    -- Move focus to the next window
    , ((modm,               xK_k     ), windows W.focusUp  )    -- Move focus to the previous window
    , ((modm,               xK_m     ), windows W.focusMaster  )    -- Move focus to the master window
    , ((modm,               xK_Return), windows W.swapMaster)    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )    -- Swap the focused window with the previous window
    , ((modm,               xK_h     ), sendMessage Shrink)    -- Shrink the master area
    , ((modm,               xK_l     ), sendMessage Expand)    -- Expand the master area
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)    -- Push window back into tiling
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))    -- Increment the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))    -- Deincrement the number of windows in the master area
    , ((modm              , xK_b     ), sendMessage ToggleStruts)    -- Toggle the status bar gap
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))    -- Quit xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")    -- Restart xmonad
    , ((modm .|. shiftMask, xK_Print     ), spawn  "scrot -e 'mv $f ~/Pictures/Screenshots/%Y-%m-%d-%H-%M-%S.png 2>/dev/null'")    -- Screenshot
    , ((modm .|. shiftMask, xK_i     ), spawn "amixer set Master Front 3+")    -- Multimedia
    , ((modm .|. shiftMask, xK_d     ), spawn "amixer set Master Front 3-")    -- Multimedia
    , ((modm .|. shiftMask, xK_m     ),spawn "amixer -q set Master toggle")
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Mouse bindings bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    , ((modm .|. shiftMask, button1), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

-- Layout definitions & modifiers
myLayout = ( tiled ||| Mirror tiled ||| grid ||| bsp ||| Full )
  where
     tiled = renamed [Replace "Tall"]   $ spacing 4 $ ResizableTall 1 (3/100) (1/2) []
     grid = renamed  [Replace "Grid"]   $ spacing 6 $ Grid (16/10)
     bsp = renamed   [Replace "BSP"]    $ spacing 8 $ emptyBSP
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

-- Manage hook defining various window rules
myManageHook = composeAll
     [  className =? "firefox"     --> doShift "<action=xdotool key super+2>www</action>"
      , className =? "Gimp"        --> doFloat
      , className =? "mpv"        --> doFloat
      , className =? "Gimp"        --> doShift "<action=xdotool key super+9>gimp</action>"
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     ]

-- Log Hook Definition: Custom Xmobar Output + Update Pointer Hook
myLogHook :: Handle -> X ()
myLogHook xmproc = dynamicLogWithPP xmobarPP
                     { ppOutput = hPutStrLn xmproc
                     , ppCurrent = xmobarColor "#83a598" "" . wrap "[" "]"   -- Visible but not current workspace
                     , ppTitle = xmobarColor "#b3afc2" "" . shorten 50       -- Title of active window in xmobar
                     } 

-- Main Xmonad entry point
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar"
    xmonad $ myConfig xmproc  

-- Config
myConfig xmproc = withNavigation2DConfig def {
        defaultTiledNavigation = centerNavigation
      } $ def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = avoidStruts $ desktopLayoutModifiers $ smartBorders $ mkToggle (NOBORDERS ?? FULL ?? EOT) myLayout,
        manageHook         = myManageHook <+> manageHook desktopConfig,
        handleEventHook    = myEventHook <+> handleEventHook desktopConfig,
        logHook            = (myLogHook xmproc) <+>fadeInactiveLogHook 0.90 <+> logHook desktopConfig,
        startupHook        = myStartupHook
      }
