module XList ( XMList(li_gen, li_itemHeight)
             , createList
             , clearList
             , resetList
             ) where

import Graphics.X11 ( setForeground, setBackground, fillRectangle
                    , drawRectangle, Pixmap, Position, Dimension
                    , createPixmap, defaultScreenOfDisplay
                    , defaultDepthOfScreen, freePixmap, copyArea
                    , xK_Up, xK_Down
                    )

import Data.Bool (bool)
import Data.Function ((&))
import Data.Maybe (isJust)
import qualified Data.Foldable as F (toList)
import qualified Data.Sequence as S
import qualified Data.Map as M (fromList, (!))

import qualified Control.Monad.Trans.Reader as RT (ReaderT, ask
                                                  , runReaderT, reader
                                                  )
import Control.Monad.Reader (liftIO, Reader, runReader, ask)
import Control.Monad (when, sequence, liftM2, mfilter, liftM)

import XMenuGlobal
import XContext
import XElementClass
import XManagerClass
import XEvent

data XMList a = XMList { li_gen         :: XMGenProps
                       , li_events      :: XMElEventMap (XMList a)
                       , li_items       :: XMItems a
                       , li_selected    :: Maybe Int
                       , li_viewY       :: Position
                       , li_itemHeight  :: Dimension
                       }

li_length = S.length . li_items

createList :: (XMElementClass a) => String -> Position -> Position
                                 -> Dimension -> Dimension -> Dimension
                                 -> [XMenuGlobal -> a]
                                 -> Reader XMenuGlobal (XMList a)
createList name x y w h ih lst = ((uncurry . liftM2 $ (,))
                               $ ((defaultGenProps name x y w h), ask))
                             >>= \(gp, xmg) -> return
                                             . (flip setElements)
                                               (S.fromList
                                                 . map (xmg &) $ lst)
                                        $ XMList gp emptyEventMap S.empty
                                                 Nothing 0 ih

instance XEManagerClass XMList where
    getElements = li_items
    getFocus = li_selected
    setFocus list foc = list { li_selected = foc }
    setElements list els = processList $ list { li_items = els }
    getMap _ = Nothing
    setMap xem _ = xem
    getEventMap _ = emptyEventMap

instance (XMElementClass a) => XMElementClass (XMList a) where
    drawContents = drawList

    getGenProps = li_gen
    setGenProps list gp = list { li_gen = gp }

    getElEventMap = li_events

    sendKeyInput list ks = (\(lst, rdrw) -> liftM (flip (,) rdrw)
                                          $ runElEvent lst (XMKeyEvent ks))
        . bool ((list, False))
               ((flip (,)) True . maybe (resetList list $ Just 0)
                                       walkList $ getFocus list)
        $ (ks == xK_Up || ks == xK_Down)

        where ksToDir = M.fromList [ (xK_Up,     Backward)
                                   , (xK_Down,   Forward)
                                   ] M.! ks

              focIsLast = isJust . mfilter (-1 + li_length list ==)
                        . getFocus $ list

              focIsFirst = isJust . mfilter (0 ==)
                         . getFocus $ list

              walkList foc = bool ((flip keyScrollToFocus) ksToDir
                                  . changeFocus list $ ksToDir)
                                  list
                           . direction (foc + 1 == li_length list)
                                       (foc == 0)
                           $ ksToDir

processList :: (XMElementClass a) => XMList a -> XMList a
processList list = list { li_items = fst
                                   . S.foldlWithIndex
    (\(lst, lastY) _ item ->
        let h' = fromIntegral . gp_height . getGenProps $ item
            h = bool h' (li_itemHeight list) (h' == 0)
            xP = fromIntegral . gp_xPad $ lgp
        in (lst S.>< S.fromList [updateGenProps item $ \gp ->

                             gp { gp_x = 0
                                , gp_y = lastY
                                , gp_width = gp_width lgp - 2 * xP
                                , gp_height = h
                                , gp_overridesEsc = False
                            }
                        ]
        , lastY + fromIntegral h
    )) (S.empty, fromIntegral (gp_yPad lgp)) $ li_items list }
    where lgp = li_gen list

drawList :: (XMElementClass a) => XMContext -> XMList a -> Dimension
                               -> Dimension -> Bool
                               -> RT.ReaderT XMenuData IO ()
drawList context list w h _ =
        mapM_ (\(i, e) -> drawElement context e
                        . isJust
                        . mfilter (i==)
                        . getFocus $ list)
      . map (\(i, e) -> (i, updateGenProps e
                      $ \gp -> gp { gp_y = gp_y gp - li_viewY list } ))
      . F.toList
      . S.takeWhileL ((\gp -> gp_y gp < li_viewY list + fromIntegral h)
                      . getGenProps . snd)
      . S.dropWhileL ((\gp -> gp_y gp + (fromIntegral . gp_height $ gp)
                           <= li_viewY list)
                      . getGenProps . snd)
      . S.mapWithIndex (,)
      . li_items $ list

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
          scrollDown = list { li_viewY = gp_y gpFoc
                                       + fromIntegral (2 * gp_yPad gpFoc)
                                       + fromIntegral (gp_height gpFoc)
                                       - fromIntegral (gp_height gpList) }
          scrollUp = list { li_viewY = gp_y gpFoc }

clearList :: (XMElementClass a) => XMList a -> XMList a
clearList = (flip setElements) S.empty
          . (flip setFocus) Nothing

resetList :: (XMElementClass a) => XMList a -> Maybe Int -> XMList a
resetList lst foc = (\lst' -> lst' { li_viewY = 0 })
                  . setFocus lst . bool foc Nothing . isEmpty $ lst

insertElement :: (XMElementClass a) => XMList a -> a -> Int -> XMList a
insertElement list el pos = processList
                          . setElements list
                          $ S.take pos (li_items list)
                       S.>< (S.singleton el) S.>< S.drop pos (li_items list)

