module XMenuGlobal
    ( XMenuOpts(..)
    , XMenuData(..)
    , XMenuGlobal(..)
    , XMGenProps(..)
    , createFont
    ) where

import Graphics.X11 ( Dimension, Pixel, FontStruct, Font
                    , Screen, ScreenNumber, Display, Window
                    , Position
                    )

createFont :: String -> Int -> String
createFont name size = "-*-" ++ name ++ "-*-*-*-*-"
                    ++ (show size) ++ "-*-*-*-*-*-*-*"

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
                             , gp_background    :: Bool
                             , gp_fontStruct    :: FontStruct
                             , gp_focused       :: Bool
                             , gp_canFocus      :: Bool
                             , gp_overridesEsc  :: Bool
                             , gp_fgFocColor    :: Pixel
                             , gp_bgFocColor    :: Pixel
                             }

