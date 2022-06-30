module XList ( XMList(li_gen, li_itemHeight)
             , createList
             ) where

import XMenuGlobal
import XContext
import XElementClass
import XManagerClass
import Graphics.X11 ( setForeground, setBackground, fillRectangle
                    , drawRectangle, Pixmap, Position, Dimension
                    , createPixmap, defaultScreenOfDisplay
                    , defaultDepthOfScreen, freePixmap, copyArea
                    )
import qualified Control.Monad.Trans.Reader as RT (ReaderT, ask, runReaderT, reader)
import Control.Monad.Reader (liftIO, Reader, runReader, ask)
import Control.Monad (when, sequence, liftM2)
import Data.Bool (bool)
import Data.Function ((&))

data XMList a = XMList { li_gen         :: XMGenProps
                       , li_items       :: [a]
                       , li_selected    :: Maybe Int
                       , li_viewY       :: Position
                       , li_itemHeight  :: Dimension
                       }

createList :: (XMElementClass a) => Position -> Position -> Dimension
                                  -> Dimension -> Dimension
                                  -> [XMenuGlobal -> a]
                                  -> Reader XMenuGlobal (XMList a)
createList x y w h ih lst = ((uncurry . liftM2 $ (,))
                          $ ((defaultGenProps x y w h), ask))
                        >>= \(gp, xmg) -> return
                                        . (flip setElements) (map (xmg &) lst)
                                        $ XMList gp [] Nothing 0 ih

instance XEManagerClass XMList where
    getElements = li_items
    getFocus = li_selected
    setFocus list foc = list { li_selected = foc }
    setElements list els = processList list { li_items = els }

instance (XMElementClass a) => XMElementClass (XMList a) where
    drawElement = drawList
    getGenProps = li_gen
    setGenProps list gp = list { li_gen = gp }
    sendKeyInput list (kc, _)
        | kc == 111 || kc == 116    = return
                                    . changeFocus list
                                    . bool Backward Forward
                                    $ kc == 116
        | otherwise                 = return list

processList :: (XMElementClass a) => XMList a -> XMList a
processList list = list { li_items = fst . foldl (\(lst, lastY) item ->
    let h' = fromIntegral . gp_height . getGenProps $ item
        h = bool h' (li_itemHeight list) (h' == 0)
        xP = fromIntegral . gp_xPad . li_gen $ list
    in (lst ++ [updateGenProps item $ \gp ->
                         gp { gp_x = 0
                            , gp_y = lastY
                            , gp_width = gp_width (li_gen list) - 2 * xP
                            , gp_height = h
                            , gp_overridesEsc = False
                            }
             ]
    , (lastY + fromIntegral h)-- . fromIntegral . gp_height . getGenProps $ item
    )) ([], fromIntegral . gp_yPad . li_gen $ list) $ li_items list }

drawList :: (XMElementClass a) => XMContext -> XMList a
                               -> RT.ReaderT XMenuData IO ()
drawList context list = RT.ask >>= \xmdata -> liftIO $ do
        let display = g_display xmdata
        let lg = li_gen list
        let (li_width, li_height) = (gp_width lg, gp_height lg)
        let (fgColor, bgColor) = getColorsDynamic lg
        let (li_x, li_y) = (gp_x lg, gp_y lg)
        let (drawable, gc) = (c_drawable context, c_gc context)

        pixmap <- createPixmap display drawable li_width li_height
                $ defaultDepthOfScreen
                . defaultScreenOfDisplay
                $ display

        when (gp_background . li_gen $ list) $ do
            setForeground display gc bgColor
            fillRectangle display pixmap gc 0 0 li_width li_height

        mapM_ (\item -> do
                putStrLn . show . gp_focused . getGenProps $ item
                (flip RT.runReaderT) xmdata
                  . drawElement context { c_drawable = pixmap } $ item)
            . filter ((\gp -> gp_y gp + (fromIntegral . gp_height $ gp)
                       > li_viewY list
                      && gp_y gp
                       < (fromIntegral . gp_height . li_gen $ list)
                     ) . getGenProps)
            . li_items
            $ list

        when (gp_border . li_gen $ list) $ do
            setForeground display gc fgColor
            (flip RT.runReaderT) xmdata $ drawBorder (createContext pixmap gc)
                                                     0 0 li_width li_height 5


        copyArea display pixmap drawable gc 0 0 li_width li_height li_x li_y

        freePixmap display pixmap

