module XManager ( XEManager(..)
                , drawAll
                , sendKeyInputToManager
                , createManager
                , changeFocus
                , focusOverridesEsc
                , unfocus
                , XEManagerClass(..)
                ) where

import Graphics.X11 (KeyCode)
import XElement as XE
import XContext
import XMenuGlobal
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (Reader, ask, liftIO)
import Data.Bool (bool)

data XEManager a = XEManager { xem_elements   :: [a]
                             , xem_inFocus    :: Maybe Int
                             }

class XEManagerClass f where
    getElements :: (XMElementClass a) => f a -> [a]
    getFocus :: (XMElementClass a) => f a -> Maybe Int
    setFocus :: (XMElementClass a) => f a -> Maybe Int -> f a
    setElements :: (XMElementClass a) => f a -> [a] -> f a

    getElement :: (XMElementClass a) => f a -> Int -> a
    getElement xem no = (getElements xem) !! no

    replaceElement :: (XMElementClass a) => f a -> Int -> a -> f a
    replaceElement xem no el = let (p1, p2) = splitAt no $ getElements xem
                               in setElements xem $ p1 ++ [el]
                                                ++ (bool (tail p2) []
                                                   (length p2 == 0))

instance XEManagerClass (XEManager) where
    getElements = xem_elements
    setElements xem els = xem { xem_elements = els }
    getFocus = xem_inFocus
    setFocus xem foc = xem { xem_inFocus = foc }

createManager :: (XMElementClass b) => [XMenuGlobal -> b]
              -> Reader XMenuGlobal (XEManager b)
createManager xels = (\xmap -> return $ XEManager xmap Nothing)
                   . (\xmglobal -> (map (\x -> x xmglobal) xels) ) =<< ask

sendKeyInputToManager :: (XEManagerClass a, XMElementClass b) => a b
                      -> (KeyCode, String) -> IO (a b)
sendKeyInputToManager xem kdata = maybe (return xem)
                                        (\foc -> return
                                               . replaceElement xem foc
                                             =<< (((flip sendKeyInput) kdata)
                                               . getElement xem $ foc))
                                        (getFocus xem)

drawAll :: (XEManagerClass a, XMElementClass b) => a b -> XMContext
        -> ReaderT XMenuData IO ()
drawAll xem ctx = drawAll' ctx . getElements $ xem
    where drawAll' ctx (x:xs) = do
            drawElement ctx x
            drawAll' ctx xs
          drawAll' _ [] = return ()

changeFocus :: (XEManagerClass a, XMElementClass b) => a b -> a b
changeFocus xem = case getElements xem of
    []          -> xem
    otherwise   -> let foc = getFocus xem
                   in changeFocus' foc xem (maybe 0 (nextElement xem) foc)
    where changeFocus' foc xem' curr =
               bool (changeFocus' foc xem'
                    . nextElement xem' $ curr)
                     (bool (removeOldFocus foc
                          . setNewFocus curr
                          $ XManager.setFocus xem' $ Just curr) xem'
                          $ maybe False (curr ==) foc)
                    $ canFocus (getElement xem' curr) || case foc of
                        Just foc'   -> foc' == curr
                        Nothing     -> curr + 1 == length (getElements xem')
          nextElement xem' curr =
                    bool (curr + 1) 0 (curr + 1 == length (getElements xem'))
          removeOldFocus foc xem' = maybe xem'
                                          (\foc' -> replaceElement xem' foc'
                                                  . (flip XE.setFocus False)
                                                  . getElement xem'
                                                  $ foc')
                                          foc
          setNewFocus foc xem' = replaceElement xem' foc
                               . (flip XE.setFocus True)
                               . getElement xem'
                               $ foc

focusOverridesEsc :: (XEManagerClass a, XMElementClass b) => a b -> Bool
focusOverridesEsc xem =
    maybe False (\foc' -> gp_overridesEsc
                        $ getGenProps (getElement xem foc'))
          (getFocus xem)

unfocus :: (XEManagerClass a, XMElementClass b) => a b -> a b
unfocus xem =
    maybe xem (\foc' -> (flip XManager.setFocus) Nothing
                      . replaceElement xem foc'
                      . (flip XE.setFocus False)
                      . getElement xem
                      $ foc')
          (getFocus xem)
