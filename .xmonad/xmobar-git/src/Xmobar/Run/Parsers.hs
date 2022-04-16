{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Run.Parsers
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  portable
--
-- Parsing for template substrings
--
-----------------------------------------------------------------------------

module Xmobar.Run.Parsers ( parseString
                          , colorComponents
                          , Segment
                          , FontIndex
                          , Box(..)
                          , BoxBorder(..)
                          , BoxOffset(..)
                          , BoxMargins(..)
                          , TextRenderInfo(..)
                          , Widget(..)) where

import Control.Monad (guard, mzero)
import Data.Maybe (fromMaybe)
import Data.Int (Int32)
import Text.ParserCombinators.Parsec
import Text.Read (readMaybe)
import Foreign.C.Types (CInt)

import Xmobar.Config.Types
import Xmobar.Run.Actions

data Widget = Icon String | Text String | Hspace Int32 deriving Show

data BoxOffset = BoxOffset Align Int32 deriving (Eq, Show)
-- margins: Top, Right, Bottom, Left
data BoxMargins = BoxMargins Int32 Int32 Int32 Int32 deriving (Eq, Show)
data BoxBorder = BBTop
               | BBBottom
               | BBVBoth
               | BBLeft
               | BBRight
               | BBHBoth
               | BBFull
                 deriving ( Read, Eq, Show )
data Box = Box BoxBorder BoxOffset CInt String BoxMargins deriving (Eq, Show)
data TextRenderInfo =
    TextRenderInfo { tColorsString   :: String
                   , tBgTopOffset    :: Int32
                   , tBgBottomOffset :: Int32
                   , tBoxes          :: [Box]
                   } deriving Show
type FontIndex   = Int

type Segment = (Widget, TextRenderInfo, FontIndex, Maybe [Action])

-- | Runs the string parser
parseString :: Config -> String -> IO [Segment]
parseString c s =
    case parse (stringParser ci 0 Nothing) "" s of
      Left  _ -> return [(Text $ "Could not parse string: " ++ s
                          , ci
                          , 0
                          , Nothing)]
      Right x -> return (concat x)
    where ci = TextRenderInfo (fgColor c) 0 0 []

-- | Splits a colors string into its two components
colorComponents :: Config -> String -> (String, String)
colorComponents conf c =
  case break (==',') c of
    (f,',':b) -> (f, b)
    (f,    _) -> (f, bgColor conf)

allParsers :: TextRenderInfo
           -> FontIndex
           -> Maybe [Action]
           -> Parser [Segment]
allParsers c f a =  textParser c f a
                <|> try (iconParser c f a)
                <|> try (hspaceParser c f a)
                <|> try (rawParser c f a)
                <|> try (actionParser c f a)
                <|> try (fontParser c a)
                <|> try (boxParser c f a)
                <|> colorParser c f a

-- | Gets the string and combines the needed parsers
stringParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [[Segment]]
stringParser c f a = manyTill (allParsers c f a) eof

-- | Parses a maximal string without markup.
textParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
textParser c f a = do s <- many1 $
                            noneOf "<" <|>
                              try (notFollowedBy' (char '<')
                                    (try (string "fc=")  <|>
                                     try (string "box")  <|>
                                     try (string "fn=")  <|>
                                     try (string "action=") <|>
                                     try (string "/action>") <|>
                                     try (string "icon=") <|>
                                     try (string "hspace=") <|>
                                     try (string "raw=") <|>
                                     try (string "/fn>") <|>
                                     try (string "/box>") <|>
                                     string "/fc>"))
                      return [(Text s, c, f, a)]

-- | Parse a "raw" tag, which we use to prevent other tags from creeping in.
-- The format here is net-string-esque: a literal "<raw=" followed by a
-- string of digits (base 10) denoting the length of the raw string,
-- a literal ":" as digit-string-terminator, the raw string itself, and
-- then a literal "/>".
rawParser :: TextRenderInfo
          -> FontIndex
          -> Maybe [Action]
          -> Parser [Segment]
rawParser c f a = do
  string "<raw="
  lenstr <- many1 digit
  char ':'
  case reads lenstr of
    [(len,[])] -> do
      guard ((len :: Integer) <= fromIntegral (maxBound :: Int))
      s <- count (fromIntegral len) anyChar
      string "/>"
      return [(Text s, c, f, a)]
    _ -> mzero

-- | Wrapper for notFollowedBy that returns the result of the first parser.
--   Also works around the issue that, at least in Parsec 3.0.0, notFollowedBy
--   accepts only parsers with return type Char.
notFollowedBy' :: Parser a -> Parser b -> Parser a
notFollowedBy' p e = do x <- p
                        notFollowedBy $ try (e >> return '*')
                        return x

iconParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
iconParser c f a = do
  string "<icon="
  i <- manyTill (noneOf ">") (try (string "/>"))
  return [(Icon i, c, f, a)]

hspaceParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
hspaceParser c f a = do
  string "<hspace="
  pVal <- manyTill digit (try (string "/>"))
  return [(Hspace (fromMaybe 0 $ readMaybe pVal), c, f, a)]

actionParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
actionParser c f act = do
  string "<action="
  command <- choice [between (char '`') (char '`') (many1 (noneOf "`")),
                   many1 (noneOf ">")]
  buttons <- (char '>' >> return "1") <|> (space >> spaces >>
    between (string "button=") (string ">") (many1 (oneOf "12345")))
  let a = Spawn (toButtons buttons) command
      a' = case act of
        Nothing -> Just [a]
        Just act' -> Just $ a : act'
  s <- manyTill (allParsers c f a') (try $ string "</action>")
  return (concat s)

toButtons :: String -> [Button]
toButtons = map (\x -> read [x])

-- | Parsers a string wrapped in a color specification.
colorParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
colorParser (TextRenderInfo _ _ _ bs) f a = do
  c <- between (string "<fc=") (string ">") colors
  let colorParts = break (==':') c
  let (ot,ob) = case break (==',') (Prelude.drop 1 $ snd colorParts) of
             (top,',':btm) -> (top, btm)
             (top,      _) -> (top, top)
  s <- manyTill
       (allParsers (TextRenderInfo (fst colorParts) (fromMaybe (-1) $ readMaybe ot) (fromMaybe (-1) $ readMaybe ob) bs) f a)
       (try $ string "</fc>")
  return (concat s)

-- | Parses a string wrapped in a box specification.
boxParser :: TextRenderInfo -> FontIndex -> Maybe [Action] -> Parser [Segment]
boxParser (TextRenderInfo cs ot ob bs) f a = do
  c <- between (string "<box") (string ">") (option "" (many1 (alphaNum <|> char '=' <|> char ' ' <|> char '#' <|> char ',')))
  let b = Box BBFull (BoxOffset C 0) 1 cs (BoxMargins 0 0 0 0)
  let g = boxReader b (words c)
  s <- manyTill
       (allParsers (TextRenderInfo cs ot ob (g : bs)) f a)
       (try $ string "</box>")
  return (concat s)

boxReader :: Box -> [String] -> Box
boxReader b [] = b
boxReader b (x:xs) = do
  let (param,val) = case break (=='=') x of
                 (p,'=':v) -> (p, v)
                 (p,    _) -> (p, "")
  boxReader (boxParamReader b param val) xs

boxParamReader :: Box -> String -> String -> Box
boxParamReader b _ "" = b
boxParamReader (Box bb off lw fc mgs) "type" val =
  Box (fromMaybe bb $ readMaybe ("BB" ++ val)) off lw fc mgs
boxParamReader (Box bb (BoxOffset alg off) lw fc mgs) "offset" (a:o) =
  Box bb (BoxOffset (fromMaybe alg $ readMaybe [a]) (fromMaybe off $ readMaybe o)) lw fc mgs
boxParamReader (Box bb off lw fc mgs) "width" val =
  Box bb off (fromMaybe lw $ readMaybe val) fc mgs
boxParamReader (Box bb off lw _ mgs) "color" val =
  Box bb off lw val mgs
boxParamReader (Box bb off lw fc mgs@(BoxMargins mt mr mb ml)) ('m':pos) val = do
  let mgs' = case pos of
         "t" -> BoxMargins (fromMaybe mt $ readMaybe val) mr mb ml
         "r" -> BoxMargins mt (fromMaybe mr $ readMaybe val) mb ml
         "b" -> BoxMargins mt mr (fromMaybe mb $ readMaybe val) ml
         "l" -> BoxMargins mt mr mb (fromMaybe ml $ readMaybe val)
         _ -> mgs
  Box bb off lw fc mgs'
boxParamReader b _ _ = b

-- | Parsers a string wrapped in a font specification.
fontParser :: TextRenderInfo -> Maybe [Action] -> Parser [Segment]
fontParser c a = do
  f <- between (string "<fn=") (string ">") colors
  s <- manyTill (allParsers c (fromMaybe 0 $ readMaybe f) a) (try $ string "</fn>")
  return (concat s)

-- | Parses a color specification (hex or named)
colors :: Parser String
colors = many1 (alphaNum <|> char ',' <|> char ':' <|> char '#')
