module XManagerClass ( XEManager(..)
                     , createManager
                     , XEManagerClass(..)
                     , XEFocusDirection(..)
                     , direction
                     ) where

import Graphics.X11 (KeyCode)
import XElementClass as XE
import XContext
import XMenuGlobal
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (Reader, ask, liftIO, mfilter)
import Data.Bool (bool)
import Data.Maybe (isJust)

data XEManager a = XEManager { xem_elements   :: [a]
                             , xem_inFocus    :: Maybe Int
                             }
data XEFocusDirection = Forward | Backward deriving (Eq)

direction :: a -> a -> XEFocusDirection -> a
direction x _ Forward = x
direction _ x Backward = x

class XEManagerClass f where
    getElements :: (XMElementClass a) => f a -> [a]
    getFocus :: (XMElementClass a) => f a -> Maybe Int
    setFocus :: (XMElementClass a) => f a -> Maybe Int -> f a
    setElements :: (XMElementClass a) => f a -> [a] -> f a

    getElement :: (XMElementClass a) => f a -> Int -> a
    getElement xem no = (getElements xem) !! no

    replaceElement :: (XMElementClass a) => f a -> Int -> a -> f a
    replaceElement xem no el = let (p1, p2) = splitAt no
                                            . getElements $ xem
                               in setElements xem . ((p1 ++ [el]) ++)
                                                  . bool (tail p2)
                                                         []
                                                  $ null p2

    sendKeyInputToManager :: (XMElementClass a) => f a
                          -> (KeyCode, String) -> IO (f a)
    sendKeyInputToManager xem kdata@(kc, _) = case getFocus xem of
        Just foc    -> bool ( return
                            . replaceElement xem foc
                          =<< ((flip sendKeyInput) kdata
                            . getElement xem $ foc)
                            )
                            ( return
                            . unfocus $ xem
                            ) $ kc == 9 && focusOverridesEsc xem

        Nothing     -> return xem

    drawAll :: (XMElementClass a) => f a -> XMContext
            -> ReaderT XMenuData IO ()
    drawAll xem ctx = drawAll' ctx . getElements $ xem
        where drawAll' ctx lst = case lst of
                        (x:xs)  -> do drawElement ctx x
                                      drawAll' ctx xs
                        []      -> return ()

    focusOverridesEsc :: (XMElementClass a) => f a -> Bool
    focusOverridesEsc xem = maybe False
                                  (\foc' -> gp_overridesEsc
                                          $ getGenProps
                                          . getElement xem
                                          $ foc')
                                  (getFocus xem)

    changeFocus :: (XMElementClass a) => f a -> XEFocusDirection -> f a
    changeFocus xem dir = case getElements xem of
        []          -> xem
        otherwise   -> let foc = getFocus xem
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
                            = bool ( removeOldFocus foc
                                   . setNewFocus curr
                                   $ XManagerClass.setFocus xem'
                                   $ Just curr) xem'
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

              xemCount xem' = length . getElements $ xem'

              removeOldFocus foc xem' = case foc of
                Nothing     -> xem'
                Just foc'   -> replaceElement xem' foc'
                             . (flip XE.setFocus False)
                             . getElement xem'
                             $ foc'

              setNewFocus foc xem' = replaceElement xem' foc
                                   . (flip XE.setFocus True)
                                   . getElement xem'
                                   $ foc

    unfocus :: (XMElementClass a) => f a -> f a
    unfocus xem = case getFocus xem of
        Just foc    -> (flip XManagerClass.setFocus) Nothing
                     . replaceElement xem foc
                     . (flip XE.setFocus False)
                     . getElement xem
                     $ foc

        Nothing     -> xem

instance XEManagerClass (XEManager) where
    getElements = xem_elements
    setElements xem els = xem { xem_elements = els }
    getFocus = xem_inFocus
    setFocus xem foc = xem { xem_inFocus = foc }

createManager :: (XMElementClass b) => [XMenuGlobal -> b]
              -> Reader XMenuGlobal (XEManager b)
createManager xels = (\xmap -> return $ XEManager xmap Nothing)
                   . (\xmglobal -> (map (\x -> x xmglobal) xels) )
                 =<< ask


