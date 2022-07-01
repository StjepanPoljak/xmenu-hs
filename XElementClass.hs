module XElementClass ( XMElementClass(..)
                     ) where

import XContext
import XMenuGlobal
import Graphics.X11 (KeyCode, copyArea, createPixmap, freePixmap
                    , setForeground, defaultScreenOfDisplay, Dimension
                    , defaultDepthOfScreen, fillRectangle)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Reader (liftIO)
import Control.Monad (when)
import Data.Bool (bool)

class XMElementClass a where
    sendKeyInput :: a -> (KeyCode, String) -> IO a
    drawContents :: XMContext -> a -> Dimension -> Dimension -> ReaderT XMenuData IO ()
    getGenProps :: a -> XMGenProps
    setGenProps :: a -> XMGenProps -> a

    canFocus :: a -> Bool
    canFocus = gp_canFocus . getGenProps

    setFocus :: a -> Bool -> a
    setFocus xmel foc = setGenProps xmel (getGenProps xmel)
                                        { gp_focused = foc }

    updateGenProps :: a -> (XMGenProps -> XMGenProps) -> a
    updateGenProps xmel f = setGenProps xmel $ f (getGenProps xmel)

    drawElement :: XMContext -> a -> ReaderT XMenuData IO ()
    drawElement context el = ask >>= \xmdata -> do
        let display = g_display xmdata
        let el_gp = getGenProps el
        let (xPad, yPad) = (gp_xPad el_gp, gp_yPad el_gp)
        let (el_w, el_h) = (gp_width el_gp, gp_height el_gp)
        let (el_x, el_y) = (gp_x el_gp, gp_y el_gp)
        let (fgColor, bgColor) = getColorsDynamic el_gp
        let (drawable, gc) = (c_drawable context, c_gc context)

        pixmap <- liftIO . createPixmap display drawable el_w el_h
                                      $ defaultDepthOfScreen
                                      . defaultScreenOfDisplay
                                      $ display

        liftIO $ bool (copyArea display drawable pixmap gc
                                (el_x + fromIntegral xPad)
                                (el_y + fromIntegral yPad)
                                el_w el_h 0 0)
                      (do setForeground display gc bgColor
                          fillRectangle display drawable gc el_x el_y el_w el_h
                          fillRectangle display pixmap gc 0 0 el_w el_h)
                      (gp_background el_gp)


        drawContents (createContext pixmap gc) el
                     (el_w - 2 * xPad) (el_h - 2 * yPad)

        when (gp_border el_gp) $ do
            liftIO . setForeground display gc $ fgColor
            drawBorder context el_x el_y el_w el_h
                       (gp_borderWidth el_gp)

        liftIO $ do
            copyArea display pixmap drawable gc 0 0
                     (el_w - 2 * xPad) (el_h - 2 * yPad)
                     (el_x + fromIntegral xPad)
                     (el_y + fromIntegral yPad)

        liftIO $ freePixmap display pixmap
