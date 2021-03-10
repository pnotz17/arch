-------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ClickableWorkspaces
-- Copyright   :  (c) Geoff deRosenroll <geoffderosenroll@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Geoff deRosenroll <geoffderosenroll@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides @clickablePP@, which when applied to the PP pretty-printer used by
-- the "XMonad.Hooks.DynamicLog" hook, will make the workspace tags clickable in
-- XMobar (for switching focus).
--
-----------------------------------------------------------------------------

module XMonad.Util.ClickableWorkspaces (
  -- * Usage
  -- $usage
  clickablePP,
  clickableRenamedPP,
  clickableWrap,

  -- * Integrations
  clickableWorkspaceNamesPP,
  clickableMarshallPP,
  clickableMarshallWorkspaceNamesPP
  ) where

import Data.Functor ((<&>))

import XMonad
import XMonad.Actions.WorkspaceNames
import XMonad.Hooks.DynamicLog (xmobarAction, xmobarRaw, PP(..))
import XMonad.Layout.IndependentScreens
import XMonad.Util.WorkspaceCompare (getWsIndex)

-- $usage
-- However you have set up your PP, apply @clickablePP@ to it, and bind the result
-- to "XMonad.Hooks.DynamicLog"\'s dynamicLogWithPP like so:
--
-- > logHook = clickablePP xmobarPP { ... } >>= dynamicLogWithPP
--
-- * Requirements:
--   * @xdotool@ on system (in path)
--   * "XMonad.Hooks.EwmhDesktops" for @xdotool@ support (see Hackage docs for setup)
--   * use of UnsafeStdinReader/UnsafeXMonadLog in xmobarrc (rather than StdinReader/XMonadLog)

-- | Wrap string with an xmobar action that uses @xdotool@ to switch to
-- workspace @i@.
clickableWrap :: Int -> String -> String
clickableWrap i ws = xmobarAction ("xdotool set_desktop " ++ show i) "1" $ xmobarRaw ws

-- | Return a function that wraps workspace names in an xmobar action that
-- switches to that workspace. That workspace name must be exactly as
-- configured in 'XMonad.Core.workspaces', so this takes an additional
-- parameter that allows renaming/marshalling of the name for display, which
-- is applied after the workspace's index is looked up.
--
-- This additionally assumes that 'XMonad.Hooks.EwmhDesktops.ewmhDesktopsEventHook'
-- isn't configured to change the workspace order. We might need to add an
-- additional parameter if anyone needs that.
getClickable :: (WorkspaceId -> String) -> X (WorkspaceId -> String)
getClickable ren = do
  wsIndex <- getWsIndex
  return $ \ws -> case wsIndex ws of
                    Just idx -> clickableWrap idx (ren ws)
                    Nothing -> ws

-- | Apply clickable wrapping to all workspace fields in given PP.
clickablePP :: PP -> X PP
clickablePP = clickableRenamedPP id

-- | Alternative to 'clickablePP' that allows changing the visible workspace
-- name. Useful for integration with modules that change workspace names, such
-- as "XMonad.Layout.IndependentScreens" and "XMonad.Actions.WorkspaceNames".
-- See "XMonad.Util.ClickableWorkspaces.Integrations".
clickableRenamedPP :: (WorkspaceId -> String) -> PP -> X PP
clickableRenamedPP ren pp = do
  clickable <- getClickable ren
  return $
    pp { ppCurrent         = ppCurrent pp . clickable
       , ppVisible         = ppVisible pp . clickable
       , ppHidden          = ppHidden pp . clickable
       , ppHiddenNoWindows = ppHiddenNoWindows pp . clickable
       , ppVisibleNoWindows= ppVisibleNoWindows pp <&> (. clickable)
       , ppUrgent          = ppUrgent pp . clickable
       }

-- | Integration with "XMonad.Actions.WorkspaceNames".
clickableWorkspaceNamesPP :: PP -> X PP
clickableWorkspaceNamesPP pp = do
    rename <- getWorkspaceNames
    clickableRenamedPP rename pp

-- | Integration with "XMonad.Layout.IndependentScreens".
clickableMarshallPP :: ScreenId -> PP -> X PP
clickableMarshallPP s pp =
    clickableRenamedPP unmarshallW pp{ ppSort = marshallSort s <$> ppSort pp }

-- | Integration with both "XMonad.Actions.WorkspaceNames" and
-- "XMonad.Layout.IndependentScreens".
clickableMarshallWorkspaceNamesPP :: ScreenId -> PP -> X PP
clickableMarshallWorkspaceNamesPP s pp = do
    rename <- getWorkspaceNames
    clickableRenamedPP (unmarshallW . rename) pp{ ppSort = marshallSort s <$> ppSort pp }
