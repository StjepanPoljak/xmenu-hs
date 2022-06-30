module XMenuGlobal
    ( XMenuOpts(..)
    , XMenuData(..)
    , XMenuGlobal(..)
    , XMGenProps(..)
    , createFont
    , getColorsDynamic
    , defaultGenProps
    ) where

import Graphics.X11 ( Dimension, Pixel, FontStruct, Font
                    , Screen, ScreenNumber, Display, Window
                    , Position
                    )
import Data.Bool (bool)
import Control.Monad.Reader (ask, Reader)

createFont :: String -> Int -> String
createFont name size = "-*-" ++ name ++ "-*-*-*-*-"
                    ++ (show size) ++ "-*-*-*-*-*-*-*"

getColorsDynamic :: XMGenProps -> (Pixel, Pixel)
getColorsDynamic xgen = bool ((gp_fgColor xgen, gp_bgColor xgen))
                             ((gp_fgFocColor xgen, gp_bgFocColor xgen))
                             (gp_focused xgen)

data XMenuOpts = XMenuOpts { g_width        :: Dimension
                           , g_height       :: Dimension
                           , g_fgColor      :: Pixel
                           , g_bgColor      :: Pixel
                           , g_font         :: String
                           , g_xPad         :: Dimension
                           , g_yPad         :: Dimension
                           , g_xMarg        :: Dimension
                           , g_yMarg        :: Dimension
                           , g_fgFocColor   :: Pixel
                           , g_bgFocColor   :: Pixel
                           }

data XMenuData = XMenuData { g_display      :: Display
                           , g_screen       :: Screen
                           , g_screenNumber :: ScreenNumber
                           , g_fontStruct   :: FontStruct
                           , g_xmenuw       :: Window
                           }

data XMenuGlobal = XMenuGlobal { g_xmopts   :: XMenuOpts
                               , g_xmdata   :: XMenuData
                               }

data XMGenProps = XMGenProps { gp_x             :: Position
                             , gp_y             :: Position
                             , gp_width         :: Dimension
                             , gp_height        :: Dimension
                             , gp_xPad          :: Dimension
                             , gp_yPad          :: Dimension
                             , gp_fgColor       :: Pixel
                             , gp_bgColor       :: Pixel
                             , gp_border        :: Bool
                             , gp_borderWidth   :: Dimension
                             , gp_background    :: Bool
                             , gp_fontStruct    :: FontStruct
                             , gp_focused       :: Bool
                             , gp_canFocus      :: Bool
                             , gp_overridesEsc  :: Bool
                             , gp_fgFocColor    :: Pixel
                             , gp_bgFocColor    :: Pixel
                             }

defaultGenProps :: Position -> Position -> Dimension -> Dimension -> Reader XMenuGlobal XMGenProps
defaultGenProps x y w h = ask >>= \(XMenuGlobal xmopts xmdata) ->
                    return $ XMGenProps x y w h (g_xPad xmopts) (g_yPad xmopts)
                             (g_fgColor xmopts) (g_bgColor xmopts) False 1
                             False (g_fontStruct xmdata) False True False
                             (g_fgFocColor xmopts) (g_bgFocColor xmopts)
