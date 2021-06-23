{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.StatusBar.PP
-- Copyright   :  (c) Don Stewart <dons@cse.unsw.edu.au>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Don Stewart <dons@cse.unsw.edu.au>
-- Stability   :  unstable
-- Portability :  unportable
--
-- xmonad calls the logHook with every internal state update, which is
-- useful for (among other things) outputting status information to an
-- external status bar program such as xmobar or dzen.
--
-- This module provides a pretty-printing abstraction and utilities that can
-- be used to customize what is logged to a status bar. See
-- "XMonad.Hooks.StatusBar" for an abstraction over starting these status
-- bars. Together these are a modern replacement for
-- "XMonad.Hooks.DynamicLog", which is now just a compatibility wrapper.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.StatusBar.PP (
    -- * Usage
    -- $usage

    -- * Build your own formatter
    PP(..), def,
    dynamicLogString,
    dynamicLogWithPP,

    -- * Example formatters
    dzenPP, xmobarPP, sjanssenPP, byorgeyPP,

    -- * Formatting utilities
    wrap, pad, trim, shorten, shorten', shortenLeft, shortenLeft',
    xmobarColor, xmobarFont, xmobarAction, xmobarBorder,
    xmobarRaw, xmobarStrip, xmobarStripTags,
    dzenColor, dzenEscape, dzenStrip, filterOutWsPP,

    -- * Internal formatting functions
    pprWindowSet,
    pprWindowSetXinerama

    ) where

import qualified XMonad.StackSet as S

import XMonad
import XMonad.Prelude

import XMonad.Util.NamedWindows
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.UrgencyHook

-- $usage
-- An example usage for this module would be:
--
-- > import XMonad
-- > import XMonad.Hooks.StatusBar
-- > import XMonad.Hooks.StatusBar.PP
-- >
-- > myPP = def { ppCurrent = xmobarColor "black" "white" }
-- > mySB = statusBarProp "xmobar" (pure myPP)
-- > main = xmonad . withEasySB mySB defToggleStrutsKey $ myConfig
--
-- Check "XMonad.Hooks.StatusBar" for more examples and an in depth
-- explanation.

-- | The 'PP' type allows the user to customize the formatting of
--   status information.
data PP = PP { ppCurrent :: WorkspaceId -> String
               -- ^ how to print the tag of the currently focused
               -- workspace
             , ppVisible :: WorkspaceId -> String
               -- ^ how to print tags of visible but not focused
               -- workspaces (xinerama only)
             , ppHidden  :: WorkspaceId -> String
               -- ^ how to print tags of hidden workspaces which
               -- contain windows
             , ppHiddenNoWindows :: WorkspaceId -> String
               -- ^ how to print tags of empty hidden workspaces
             , ppVisibleNoWindows :: Maybe (WorkspaceId -> String)
               -- ^ how to print tags of empty visible workspaces
             , ppUrgent :: WorkspaceId -> String
               -- ^ format to be applied to tags of urgent workspaces.
             , ppRename :: String -> WindowSpace -> String
               -- ^ rename/augment the workspace tag
               --   (note that @WindowSpace -> …@ acts as a Reader monad)
             , ppSep :: String
               -- ^ separator to use between different log sections
               -- (window name, layout, workspaces)
             , ppWsSep :: String
               -- ^ separator to use between workspace tags
             , ppTitle :: String -> String
               -- ^ window title format for the focused window. To display
               -- the titles of all windows—even unfocused ones—check
               -- 'XMonad.Util.Loggers.logTitles'.
             , ppTitleSanitize :: String -> String
              -- ^ escape / sanitizes input to 'ppTitle'
             , ppLayout :: String -> String
               -- ^ layout name format
             , ppOrder :: [String] -> [String]
               -- ^ how to order the different log sections. By
               --   default, this function receives a list with three
               --   formatted strings, representing the workspaces,
               --   the layout, and the current window titles,
               --   respectively. If you have specified any extra
               --   loggers in 'ppExtras', their output will also be
               --   appended to the list.  To get them in the reverse
               --   order, you can just use @ppOrder = reverse@.  If
               --   you don't want to display the current layout, you
               --   could use something like @ppOrder = \\(ws:_:t:_) ->
               --   [ws,t]@, and so on.
             , ppSort :: X ([WindowSpace] -> [WindowSpace])
               -- ^ how to sort the workspaces.  See
               -- "XMonad.Util.WorkspaceCompare" for some useful
               -- sorts.
             , ppExtras :: [X (Maybe String)]
               -- ^ loggers for generating extra information such as
               -- time and date, system load, battery status, and so
               -- on.  See "XMonad.Util.Loggers" for examples, or create
               -- your own!
             , ppOutput :: String -> IO ()
               -- ^ applied to the entire formatted string in order to
               -- output it.  Can be used to specify an alternative
               -- output method (e.g. write to a pipe instead of
               -- stdout), and\/or to perform some last-minute
               -- formatting. Note that this is only used by
               -- 'dynamicLogWithPP'; it won't work with 'dynamicLogString' or
               -- "XMonad.Hooks.StatusBar".
             }

-- | The default pretty printing options:
--
-- > 1 2 [3] 4 7 : full : title
--
-- That is, the currently populated workspaces, the current
-- workspace layout, and the title of the focused window.
instance Default PP where
  def = PP { ppCurrent          = wrap "[" "]"
           , ppVisible          = wrap "<" ">"
           , ppHidden           = id
           , ppHiddenNoWindows  = const ""
           , ppVisibleNoWindows = Nothing
           , ppUrgent           = id
           , ppRename           = pure
           , ppSep              = " : "
           , ppWsSep            = " "
           , ppTitle            = shorten 80
           , ppTitleSanitize    = xmobarStrip . dzenEscape
           , ppLayout           = id
           , ppOrder            = id
           , ppOutput           = putStrLn
           , ppSort             = getSortByIndex
           , ppExtras           = []
           }

-- | Format the current status using the supplied pretty-printing format,
--   and write it to stdout.
dynamicLogWithPP :: PP -> X ()
dynamicLogWithPP pp = dynamicLogString pp >>= io . ppOutput pp

-- | The same as 'dynamicLogWithPP', except it simply returns the status
--   as a formatted string without actually printing it to stdout, to
--   allow for further processing, or use in some application other than
--   a status bar.
dynamicLogString :: PP -> X String
dynamicLogString pp = do

    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort pp

    -- layout description
    let ld = description . S.layout . S.workspace . S.current $ winset

    -- workspace list
    let ws = pprWindowSet sort' urgents pp winset

    -- run extra loggers, ignoring any that generate errors.
    extras <- mapM (userCodeDef Nothing) $ ppExtras pp

    -- window title
    wt <- maybe (pure "") (fmap show . getName) . S.peek $ winset

    return $ sepBy (ppSep pp) . ppOrder pp $
                        [ ws
                        , ppLayout pp ld
                        , ppTitle  pp $ ppTitleSanitize pp wt
                        ]
                        ++ catMaybes extras

-- | Format the workspace information, given a workspace sorting function,
--   a list of urgent windows, a pretty-printer format, and the current
--   WindowSet.
pprWindowSet :: WorkspaceSort -> [Window] -> PP -> WindowSet -> String
pprWindowSet sort' urgents pp s = sepBy (ppWsSep pp) . map fmt . sort' $
            map S.workspace (S.current s : S.visible s) ++ S.hidden s
  where
    this     = S.currentTag s
    visibles = map (S.tag . S.workspace) (S.visible s)

    fmt w = printer pp (ppRename pp (S.tag w) w)
      where
        printer | any (\x -> (== Just (S.tag w)) (S.findTag x s)) urgents = ppUrgent
                | S.tag w == this                                         = ppCurrent
                | S.tag w `elem` visibles && isJust (S.stack w)           = ppVisible
                | S.tag w `elem` visibles                                 = liftA2 fromMaybe ppVisible ppVisibleNoWindows
                | isJust (S.stack w)                                      = ppHidden
                | otherwise                                               = ppHiddenNoWindows

pprWindowSetXinerama :: WindowSet -> String
pprWindowSetXinerama ws = "[" ++ unwords onscreen ++ "] " ++ unwords offscreen
  where onscreen  = map (S.tag . S.workspace)
                        . sortOn S.screen $ S.current ws : S.visible ws
        offscreen = map S.tag . filter (isJust . S.stack)
                        . sortOn S.tag $ S.hidden ws

-- | Wrap a string in delimiters, unless it is empty.
wrap :: String  -- ^ left delimiter
     -> String  -- ^ right delimiter
     -> String  -- ^ output string
     -> String
wrap _ _ "" = ""
wrap l r m  = l ++ m ++ r

-- | Pad a string with a leading and trailing space.
pad :: String -> String
pad = wrap " " " "

-- | Trim leading and trailing whitespace from a string.
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- | Limit a string to a certain length, adding "..." if truncated.
shorten :: Int -> String -> String
shorten = shorten' "..."

-- | Limit a string to a certain length, adding @end@ if truncated.
shorten' :: String -> Int -> String -> String
shorten' end n xs | length xs < n = xs
                  | otherwise     = take (n - length end) xs ++ end

-- | Like 'shorten', but truncate from the left instead of right.
shortenLeft :: Int -> String -> String
shortenLeft = shortenLeft' "..."

-- | Like 'shorten'', but truncate from the left instead of right.
shortenLeft' :: String -> Int -> String -> String
shortenLeft' end n xs | l < n     = xs
                      | otherwise = end ++ drop (l - n + length end) xs
 where l = length xs

-- | Output a list of strings, ignoring empty ones and separating the
--   rest with the given separator.
sepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
sepBy sep = intercalate sep . filter (not . null)

-- | Use dzen escape codes to output a string with given foreground
--   and background colors.
dzenColor :: String  -- ^ foreground color: a color name, or #rrggbb format
          -> String  -- ^ background color
          -> String  -- ^ output string
          -> String
dzenColor fg bg = wrap (fg1++bg1) (fg2++bg2)
 where (fg1,fg2) | null fg = ("","")
                 | otherwise = ("^fg(" ++ fg ++ ")","^fg()")
       (bg1,bg2) | null bg = ("","")
                 | otherwise = ("^bg(" ++ bg ++ ")","^bg()")

-- | Escape any dzen metacharacters.
dzenEscape :: String -> String
dzenEscape = concatMap (\x -> if x == '^' then "^^" else [x])

-- | Strip dzen formatting or commands.
dzenStrip :: String -> String
dzenStrip = strip [] where
    strip keep x
      | null x              = keep
      | "^^" `isPrefixOf` x = strip (keep ++ "^") (drop 2 x)
      | '^' == head x       = strip keep (drop 1 . dropWhile (/= ')') $ x)
      | otherwise           = let (good,x') = span (/= '^') x
                              in strip (keep ++ good) x'

-- | Use xmobar escape codes to output a string with the font at the given index
xmobarFont :: Int     -- ^ index: index of the font to use (0: standard font)
           -> String  -- ^ output string
           -> String
xmobarFont index = wrap ("<fn=" ++ show index ++ ">") "</fn>"

-- | Use xmobar escape codes to output a string with given foreground
--   and background colors.
xmobarColor :: String  -- ^ foreground color: a color name, or #rrggbb format
            -> String  -- ^ background color
            -> String  -- ^ output string
            -> String
xmobarColor fg bg = wrap t "</fc>"
 where t = concat ["<fc=", fg, if null bg then "" else "," ++ bg, ">"]

-- | Encapsulate text with an action. The text will be displayed, and the
-- action executed when the displayed text is clicked. Illegal input is not
-- filtered, allowing xmobar to display any parse errors. Uses xmobar's new
-- syntax wherein the command is surrounded by backticks.
xmobarAction :: String
                -- ^ Command. Use of backticks (`) will cause a parse error.
             -> String
                -- ^ Buttons 1-5, such as "145". Other characters will cause a
                -- parse error.
             -> String
                -- ^ Displayed/wrapped text.
             -> String
xmobarAction command button = wrap l r
    where
        l = "<action=`" ++ command ++ "` button=" ++ button ++ ">"
        r = "</action>"

-- | Use xmobar box to add a border to an arbitrary string.
xmobarBorder :: String -- ^ Border type. Possible values: VBoth, HBoth, Full,
                       -- Top, Bottom, Left or Right
             -> String -- ^ color: a color name, or #rrggbb format
             -> Int    -- ^ width in pixels
             -> String -- ^ output string
             -> String
xmobarBorder border color width = wrap prefix "</box>"
  where
    prefix = "<box type=" ++ border ++ " width=" ++ show width ++ " color="
      ++ color ++ ">"

-- | Encapsulate arbitrary text for display only, i.e. untrusted content if
-- wrapped (perhaps from window titles) will be displayed only, with all tags
-- ignored. Introduced in xmobar 0.21; see their documentation. Be careful not
-- to shorten the result.
xmobarRaw :: String -> String
xmobarRaw "" = ""
xmobarRaw s  = concat ["<raw=", show $ length s, ":", s, "/>"]

-- | Strip xmobar markup, specifically the <fc>, <icon> and <action> tags and
-- the matching tags like </fc>.
xmobarStrip :: String -> String
xmobarStrip = converge (xmobarStripTags ["fc","icon","action"])

converge :: (Eq a) => (a -> a) -> a -> a
converge f a = let xs = iterate f a
    in fst $ head $ dropWhile (uncurry (/=)) $ zip xs $ tail xs

xmobarStripTags :: [String] -- ^ tags
        -> String -> String -- ^ with all <tag>...</tag> removed
xmobarStripTags tags = strip [] where
    strip keep [] = keep
    strip keep x
        | rest: _ <- mapMaybe dropTag tags = strip keep rest


        | '<':xs <- x = strip (keep ++ "<") xs
        | (good,x') <- span (/= '<') x = strip (keep ++ good) x' -- this is n^2 bad... but titles have few tags
      where dropTag :: String -> Maybe String
            dropTag tag = msum [fmap dropTilClose (openTag tag `stripPrefix` x),
                                                   closeTag tag `stripPrefix` x]

    dropTilClose, openTag, closeTag :: String -> String
    dropTilClose = drop 1 . dropWhile (/= '>')
    openTag str = "<" ++ str ++ "="
    closeTag str = "</" ++ str ++ ">"

-- | Transforms a pretty-printer into one not displaying the given workspaces.
--
-- For example, filtering out the @NSP@ workspace before giving the 'PP' to
-- 'dynamicLogWithPP':
--
-- > logHook = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] $ def
--
-- Here is another example, when using "XMonad.Layout.IndependentScreens".  If
-- you have handles @hLeft@ and @hRight@ for bars on the left and right screens,
-- respectively, and @pp@ is a pretty-printer function that takes a handle, you
-- could write
--
-- > logHook = let log screen handle = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] . marshallPP screen . pp $ handle
-- >           in log 0 hLeft >> log 1 hRight
filterOutWsPP :: [WorkspaceId] -> PP -> PP
filterOutWsPP ws pp = pp { ppSort = (. filterOutWs ws) <$> ppSort pp }

-- | Settings to emulate dwm's statusbar, dzen only.
dzenPP :: PP
dzenPP = def
  { ppCurrent         = dzenColor "white" "#2b4f98" . pad
  , ppVisible         = dzenColor "black" "#999999" . pad
  , ppHidden          = dzenColor "black" "#cccccc" . pad
  , ppHiddenNoWindows = const ""
  , ppUrgent          = dzenColor "red" "yellow" . pad
  , ppWsSep           = ""
  , ppSep             = ""
  , ppLayout          = dzenColor "black" "#cccccc"
                          . (\x -> pad $ case x of
                              "TilePrime Horizontal" -> "TTT"
                              "TilePrime Vertical"   -> "[]="
                              "Hinted Full"          -> "[ ]"
                              _                      -> x
                            )
  , ppTitle           = ("^bg(#324c80) " ++) . dzenEscape
  }

-- | Some nice xmobar defaults.
xmobarPP :: PP
xmobarPP = def { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
               , ppTitle   = xmobarColor "green" "" . shorten 40
               , ppVisible = wrap "(" ")"
               , ppUrgent  = xmobarColor "red" "yellow"
               }

-- | The options that sjanssen likes to use with xmobar, as an
-- example.  Note the use of 'xmobarColor' and the record update on
-- 'def'.
sjanssenPP :: PP
sjanssenPP = def { ppCurrent = xmobarColor "white" "black"
                 , ppTitle   = xmobarColor "#00ee00" "" . shorten 120
                 }

-- | The options that byorgey likes to use with dzen, as another example.
byorgeyPP :: PP
byorgeyPP = def { ppHiddenNoWindows = showNamedWorkspaces
                , ppHidden          = dzenColor "black" "#a8a3f7" . pad
                , ppCurrent         = dzenColor "yellow" "#a8a3f7" . pad
                , ppUrgent          = dzenColor "red" "yellow" . pad
                , ppSep             = " | "
                , ppWsSep           = ""
                , ppTitle           = shorten 70
                , ppOrder           = reverse
                }
 where
  showNamedWorkspaces wsId =
    if any (`elem` wsId) ['a' .. 'z'] then pad wsId else ""
