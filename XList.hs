module XList ( XMList(..)
             ) where

import XMenuGlobal
import XContext
import XElementClass
import XManagerClass
import Graphics.X11 ( setForeground, setBackground, fillRectangle
                    , drawRectangle, Pixmap, Position
                    , createPixmap, defaultScreenOfDisplay
                    , defaultDepthOfScreen, freePixmap, copyArea
                    )
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Reader (liftIO)
import Control.Monad (when, mapM_)
import Data.Bool (bool)

data XMList a = XMList { li_gen       :: XMGenProps
                       , li_items     :: [a]
                       , li_selected  :: Maybe Int
                       , li_viewY     :: Position
                       }

instance XEManagerClass XMList where
    getElements = li_items
    getFocus = li_selected
    setFocus list foc = list { li_selected = foc }
    setElements list els = processList $ list { li_items = els }

instance (XMElementClass a) => XMElementClass (XMList a) where
    drawElement = drawList
    getGenProps = li_gen
    setGenProps list gp = list { li_gen = gp }
    sendKeyInput list (kc, _)
        | kc == 111 || kc == 116    = return
                                    . changeFocus list
                                    . bool Backward Forward
                                    $ kc == 111

processList :: (XMElementClass a) => XMList a -> XMList a
processList list = list { li_items = fst . foldl (\(lst, lastY) item ->
    ( lst ++ [updateGenProps item $ \gp ->
                      let h = fromIntegral $ gp_height (li_gen list)
                          xP = fromIntegral $ gp_xPad (li_gen list)
                      in gp { gp_x = 0
                            , gp_y = lastY + h
                            , gp_width = gp_width (li_gen list) - 2 * xP
                            , gp_overridesEsc = False
                            }
             ]
    , (lastY +) . fromIntegral . gp_width . getGenProps $ item)
    ) ([], fromIntegral . gp_yPad . li_gen $ list) $ li_items list }

drawList :: (XMElementClass a) => XMContext -> XMList a
                               -> ReaderT XMenuData IO ()
drawList context list = ask >>= \xmdata -> liftIO $ do
        let display = g_display xmdata
        let lg = li_gen list
        let (li_width, li_height) = (gp_width lg, gp_height lg)
        let (fgColor, bgColor) = getColorsDynamic lg
        let (li_x, li_y) = (gp_x lg, gp_y lg)

        pixmap <- createPixmap display drawable li_width li_height
                $ defaultDepthOfScreen
                . defaultScreenOfDisplay
                $ display

        mapM_ ((flip runReaderT) xmdata
                . drawElement context { c_drawable = pixmap })
            . filter ((\gp -> gp_y gp + (fromIntegral . gp_width $ gp)
                        > li_viewY list
                       && gp_y gp
                        < (fromIntegral . gp_width . li_gen $ list)
                      ) . getGenProps)
            . li_items
            $ list
    
        when (gp_background . li_gen $ list) $ do
            setForeground display gc bgColor
            fillRectangle display pixmap gc li_x li_y li_width li_height

        copyArea display pixmap drawable gc li_x li_y li_width li_height 0 0

        freePixmap display pixmap

    where XMContext drawable gc c_w c_h = context

