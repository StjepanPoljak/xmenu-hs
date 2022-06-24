module XMenuGlobal
    ( XMenuOpts(..)
    , XMenuData(..)
    , XMenuGlobal(..)
    ) where

import Graphics.X11 ( Dimension, Pixel, FontStruct, Font
                    , Screen, ScreenNumber, Display, Window
                    )

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

