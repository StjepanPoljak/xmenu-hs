module XMenuGlobal
    ( XMenuOpts(..)
    , XMenuData(..)
    , XMenuDataM
    , XMenuGlobal(..)
    , XMGenProps(..)
--    , XMCallbacks(..)
--    , XMCallbackT
--    , XMCallback2T
    , createFont
    , getColorsDynamic
    , defaultGenProps
--    , runCallback
--    , runCallback2
--    , defaultCallbacks
    , debugColor
    , redraw
    ) where

import Graphics.X11 ( Dimension, Pixel, FontStruct, Font, allocaXEvent
                    , Screen, ScreenNumber, Display, Window
                    , Position, KeySym, exposureMask, sendEvent, expose
                    )
import Graphics.X11.Xlib.Extras (setEventType)
import Data.Bool (bool)
import Control.Monad.Reader (ask, Reader, liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad ((<=<))

debugColor :: Pixel
debugColor = 0xff0000

createFont :: String -> Int -> String
createFont name size = "-*-" ++ name ++ "-*-*-*-*-"
                    ++ (show size) ++ "-*-*-*-*-*-*-*"

getColorsDynamic :: XMGenProps -> Bool -> (Pixel, Pixel)
getColorsDynamic xgen focd = bool ((gp_fgColor xgen, gp_bgColor xgen))
                                  ((gp_fgFocColor xgen, gp_bgFocColor xgen))
                                  focd

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

type XMenuDataM a = ReaderT XMenuData IO a

data XMenuGlobal = XMenuGlobal { g_xmopts   :: XMenuOpts
                               , g_xmdata   :: XMenuData
                               }

data XMGenProps = XMGenProps { gp_name          :: String
                             , gp_x             :: Position
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
                             , gp_canFocus      :: Bool
                             , gp_overridesEsc  :: Bool
                             , gp_fgFocColor    :: Pixel
                             , gp_bgFocColor    :: Pixel
                             }

type XMCallbackT a = Maybe (a -> IO a)
type XMCallback2T a b = Maybe (a -> b -> IO a)

runCallback :: XMCallbackT a -> a -> IO a
runCallback (Just act) el = act el
runCallback Nothing el = return el

runCallback2 :: XMCallback2T a b -> a -> b -> IO a
runCallback2 (Just act) el el2 = act el el2
runCallback2 Nothing el _ = return el

data XMCallbacks a = XMCallbacks
        { cb_onKeyPress    :: XMCallback2T a KeySym
        , cb_onChange      :: XMCallbackT a
        }

defaultCallbacks :: XMCallbacks a
defaultCallbacks = XMCallbacks Nothing Nothing

defaultGenProps :: String -> Position -> Position -> Dimension -> Dimension
                -> Reader XMenuGlobal XMGenProps
defaultGenProps name x y w h = ask >>= \(XMenuGlobal xmopts xmdata) ->
                    return $ XMGenProps name x y w h
                             (g_xPad xmopts) (g_yPad xmopts)
                             (g_fgColor xmopts) (g_bgColor xmopts) False 1
                             False (g_fontStruct xmdata) True False
                             (g_fgFocColor xmopts) (g_bgFocColor xmopts)

redraw :: XMenuDataM ()
redraw = ask >>= \xmdata -> liftIO
               $ allocaXEvent $ \ev -> do
                    setEventType ev expose
                    sendEvent (g_display xmdata) (g_xmenuw xmdata) False
                              exposureMask ev


