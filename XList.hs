module XList ( XMList(li_gen, li_itemHeight)
             , createList, clearList, resetList
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
import qualified Data.Foldable as F (toList)
import qualified Data.Sequence as S

data XMList a = XMList { li_gen         :: XMGenProps
                       , li_cbs         :: XMCallbacks (XMList a)
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
                                        $ XMList gp defaultCallbacks S.empty
                                                 Nothing 0 ih

instance XEManagerClass XMList where
    getElements = li_items
    getFocus = li_selected
    setFocus list foc = list { li_selected = foc }
    setElements list els = processList $ list { li_items = els }

instance (XMElementClass a) => XMElementClass (XMList a) where
    drawContents = drawList

    getGenProps = li_gen
    setGenProps list gp = list { li_gen = gp }

    getCallbacks = Just . li_cbs

    sendKeyInput list (kc, str) = (\lst -> runCB2 lst (kc, str) cb_onKeyPress)
        =<< bool (return list)
                 (maybe (return . changeFocus list $ Forward)
                        (\foc -> return
                               . bool ((flip keyScrollToFocus) kcToDir
                                      . changeFocus list $ kcToDir)
                                      list
                               . direction (foc + 1 == li_length list)
                                           (foc == 0)
                               $ kcToDir) $ getFocus list)
                 (kc == 111 || kc == 116)

        where kcToDir = case kc of
                    111 -> Backward
                    116 -> Forward

              focIsLast = isJust . mfilter (-1 + li_length list ==)
                        . getFocus $ list

              focIsFirst = isJust . mfilter (0 ==)
                         . getFocus $ list

processList :: (XMElementClass a) => XMList a -> XMList a
processList list = list { li_items = fst . S.foldlWithIndex (\(lst, lastY) _ item ->
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
          scrollDown = list { li_viewY = gp_y gpFoc + fromIntegral (2 * gp_yPad gpFoc)
                                       + fromIntegral (gp_height gpFoc)
                                       - fromIntegral (gp_height gpList) }
          scrollUp = list { li_viewY = gp_y gpFoc }

clearList :: (XMElementClass a) => XMList a -> XMList a
clearList = (flip setElements) S.empty
          . (flip setFocus) Nothing

resetList :: (XMElementClass a) => XMList a -> XMList a
resetList = (\lst -> lst { li_viewY = 0 }) . (flip setFocus) Nothing

insertElement :: (XMElementClass a) => XMList a -> a -> Int -> XMList a
insertElement list el pos = processList
                          . setElements list
                          $ S.take pos (li_items list)
                       S.>< (S.singleton el) S.>< S.drop pos (li_items list)

