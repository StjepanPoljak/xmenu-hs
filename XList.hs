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
import qualified Control.Monad.Trans.Reader as RT (ReaderT, ask
                                                  , runReaderT, reader
                                                  )
import Control.Monad.Reader (liftIO, Reader, runReader, ask)
import Control.Monad (when, sequence, liftM2, mfilter)
import Data.Bool (bool)
import Data.Function ((&))
import Data.Maybe (isJust)

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
                                    . bool ((flip keyScrollToFocus) kcToDir
                                            . changeFocus list $ kcToDir)
                                           list
                                    . direction focIsLast focIsFirst $ kcToDir

        | otherwise                 = return list
        where kcToDir = case kc of
                    111 -> Backward
                    116 -> Forward
              focIsLast = isJust . mfilter (-1 + length (li_items list) ==) . getFocus $ list
              focIsFirst = isJust . mfilter (0 ==) . getFocus $ list

processList :: (XMElementClass a) => XMList a -> XMList a
processList list = list { li_items = fst . foldl (\(lst, lastY) item ->
    let h' = fromIntegral . gp_height . getGenProps $ item
        h = bool h' (li_itemHeight list) (h' == 0)
        xP = fromIntegral . gp_xPad $ lgp
    in (lst ++ [updateGenProps item $ \gp ->

                         gp { gp_x = 0
                            , gp_y = lastY
                            , gp_width = gp_width lgp - 2 * xP
                            , gp_height = h
                            , gp_overridesEsc = False
                            }
             ]
    , lastY + fromIntegral h
    )) ([], fromIntegral (gp_yPad lgp)) $ li_items list }
    where lgp = li_gen list

drawList :: (XMElementClass a) => XMContext -> XMList a -> Dimension
                               -> Dimension -> RT.ReaderT XMenuData IO ()
drawList context list w h = mapM_ (drawElement context)
                          . map ((flip updateGenProps)
                                 (\gp -> gp { gp_y = gp_y gp - li_viewY list }))
                          . filter ((\gp -> gp_y gp
                                          + (fromIntegral . gp_height $ gp)
                                          > li_viewY list
                                         && gp_y gp
                                          < li_viewY list + (fromIntegral h)
                                    ) . getGenProps)
                          . li_items
                          $ list

isFocusOutside :: (XMElementClass a) => XMList a -> Bool
isFocusOutside list = isJust
                    . mfilter (\foc -> let elgp = getGenProps
                                                . getElement list $ foc
                                           ely = gp_y elgp
                                           elh = gp_height elgp
                                           elyh = elh + fromIntegral ely
                                        in crossedLower elyh
                                        || crossedUpper ely)
                    . getFocus $ list
    where crossedLower elyh = elyh - fromIntegral (li_viewY list)
                            > gp_height (li_gen list)
          crossedUpper ely = ely - li_viewY list < 0

keyScrollToFocus :: (XMElementClass a) => XMList a -> XEFocusDirection
                                       -> XMList a
keyScrollToFocus list dir = bool list (direction scrollDown scrollUp dir) . isFocusOutside $ list
    where Just foc = getFocus $ list
          gpFoc = getGenProps . getElement list $ foc
          gpList = li_gen list
          scrollDown = list { li_viewY = gp_y gpFoc + fromIntegral (2 * gp_yPad gpFoc)
                                       + fromIntegral (gp_height gpFoc)
                                       - fromIntegral (gp_height gpList) }
          scrollUp = list { li_viewY = gp_y gpFoc }
