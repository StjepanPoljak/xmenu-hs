module XRDB (
    getXRDBKey,
    fontColor,
    fontName,
    fontSize,
    fgColor,
    bgColor,
    errColor,
    okColor,
    warnColor,
    lowColor
) where

import qualified Data.Map as M
import XString (splitString)
import Data.Maybe (mapMaybe)

getXRDBKey :: String -> String -> String
getXRDBKey xrdbout key = (createMap $ splitString xrdbout '\n') M.! key

createMap :: [String] -> M.Map String String
createMap strl = M.fromList
               . mapMaybe (\str -> let res = splitString str '\t'
                                   in case res of
                                     [x, y]     -> Just (pruneKey x, y)
                                     _          -> Nothing)
               $ strl

    where pruneKey :: String -> String
          pruneKey "" = ""
          pruneKey [x] = [x]
          pruneKey [x, y] = [x, y]
          pruneKey (x:y:xs)
             | last xs == ':' = pruneKey $ x:y:init xs
             | (x == '*') && (y == '.') = pruneKey xs
             | otherwise = x:y:xs

getXRDBKey' :: String -> String -> String
getXRDBKey' a b = getXRDBKey b a

fontColor   = getXRDBKey' "foreground"
fontName    = getXRDBKey' "faceName"
fontSize    = getXRDBKey' "faceSize"
fgColor     = getXRDBKey' "background"
bgColor     = getXRDBKey' "color0"
errColor    = getXRDBKey' "color1"
okColor     = getXRDBKey' "color2"
warnColor   = getXRDBKey' "color3"
lowColor    = getXRDBKey' "color4"

