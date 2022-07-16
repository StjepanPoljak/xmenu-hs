module XManagerClass ( XEManager(..)
                     , createManager
                     , XEManagerClass(..)
                     , XEFocusDirection(..)
                     , direction
                     , XMItems(..)
                     ) where

import Graphics.X11 (KeyCode)
import XElementClass as XE
import XContext
import XMenuGlobal
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (Reader, ask, liftIO, mfilter)
import Data.Bool (bool)
import Data.Maybe (isJust)
import Control.Monad ((<=<))
import qualified Data.Sequence as S
import qualified Data.Foldable as F

type XMItems a = S.Seq a

data XEManager a = XEManager { xem_elements         :: XMItems a
                             , xem_inFocus          :: Maybe Int
                             }

data XEFocusDirection = Forward | Backward deriving (Eq)

direction :: a -> a -> XEFocusDirection -> a
direction x _ Forward = x
direction _ x Backward = x

class XEManagerClass f where
    getElements :: (XMElementClass a) => f a -> XMItems a
    getFocus :: (XMElementClass a) => f a -> Maybe Int
    setFocus :: (XMElementClass a) => f a -> Maybe Int -> f a
    setElements :: (XMElementClass a) => f a -> XMItems a -> f a

    setElementsFromList :: (XMElementClass a) => f a -> [a] -> f a
    setElementsFromList xem list = setElements xem (S.fromList list)

    getElement :: (XMElementClass a) => f a -> Int -> a
    getElement xem no = (getElements xem) `S.index` no

    getElementByName :: (XMElementClass a) => f a -> String -> Maybe Int
    getElementByName xem name = S.findIndexL ((==name)
                                        . gp_name
                                        . getGenProps)
                              . getElements $ xem

    modifyElementByName :: (XMElementClass a) => f a -> String
                                              -> (a -> a) -> f a
    modifyElementByName xem name f = maybe xem (\no ->
                                        replaceElement xem no
                                      . f
                                      . getElement xem
                                      $ no) $ getElementByName xem name

    replaceElement :: (XMElementClass a) => f a -> Int -> a -> f a
    replaceElement xem no el = setElements xem
                             . S.update no el
                             . getElements $ xem

    sendKeyInputToManager :: (XMElementClass a) => f a
                          -> (KeyCode, String) -> IO (f a)
    sendKeyInputToManager xem kdata@(kc, _) = maybe (return xem)
        (\foc -> case kc of
            9   -> bool (forwardKeyTo foc) (return . unfocus $ xem)
                 . focusOverridesEsc $ xem

            _   -> forwardKeyTo foc

        ) . getFocus $ xem

        where forwardFuncTo foc fn = return
                                   . replaceElement xem foc
                                 <=< fn
                                   . getElement xem $ foc
              forwardKeyTo = (flip forwardFuncTo) ((flip sendKeyInput) kdata)

    drawAll :: (XMElementClass a) => f a -> XMContext
            -> ReaderT XMenuData IO ()
    drawAll xem ctx = mapM_ (\(i, e) -> drawElement ctx e (isJust
                                                         . mfilter (i==)
                                                         . getFocus $ xem))
                    . F.toList
                    . S.mapWithIndex ((,))
                    . getElements $ xem

    focusOverridesEsc :: (XMElementClass a) => f a -> Bool
    focusOverridesEsc xem = maybe False
                                  (\foc' -> gp_overridesEsc
                                          $ getGenProps
                                          . getElement xem
                                          $ foc')
                                  (getFocus xem)

    changeFocus :: (XMElementClass a) => f a -> XEFocusDirection -> f a
    changeFocus xem dir
        | S.null . getElements $ xem = xem
        | otherwise  = let foc = getFocus xem
                       in changeFocus' foc xem dir
                        . direction (maybe 0
                                           (nextElement xem))
                                    (maybe (xemCount xem - 1)
                                           (prevElement xem))
                                    dir
                        $ foc

        where changeFocus' foc xem' dir curr
                | canFocus (getElement xem' curr) ||
                  maybe (curr + 1 == xemCount xem') (curr==) foc
                            = bool (setFocus xem' . Just $ curr)
                                   xem'
                            . isJust
                            . mfilter (curr==)
                            $ foc

                | otherwise = changeFocus' foc xem' dir
                            . nextElement xem'
                            $ curr

              nextElement xem' curr
                | curr + 1 == xemCount xem' = 0
                | otherwise                 = curr + 1

              prevElement xem' curr
                | curr == 0     = xemCount xem' - 1
                | otherwise     = curr - 1

              xemCount xem' = S.length . getElements $ xem'

    unfocus :: (XMElementClass a) => f a -> f a
    unfocus xem = setFocus xem Nothing

instance XEManagerClass (XEManager) where
    getElements = xem_elements
    setElements xem els = xem { xem_elements = els }
    getFocus = xem_inFocus
    setFocus xem foc = xem { xem_inFocus = foc }

createManager :: (XMElementClass b) => [XMenuGlobal -> b]
              -> Reader XMenuGlobal (XEManager b)
createManager xels = (\xmap -> return $ XEManager xmap Nothing)
                   . S.fromList
                   . (\xmglobal -> (map (\x -> x xmglobal) xels) )
                 =<< ask

