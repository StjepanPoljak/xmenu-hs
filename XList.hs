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
    drawContents = drawList
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

drawList :: (XMElementClass a) => XMContext -> XMList a -> Dimension
                               -> Dimension -> RT.ReaderT XMenuData IO ()
drawList context list w h = mapM_ (drawElement context)
                          . filter ((\gp -> gp_y gp
                                          + (fromIntegral . gp_height $ gp)
                                          > li_viewY list
                                         && gp_y gp
                                          < (fromIntegral h)
                                    ) . getGenProps)
                          . li_items
                          $ list

