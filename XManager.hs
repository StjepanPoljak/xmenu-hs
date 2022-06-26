module XManager ( XEManager(..)
                , drawAll
                , sendKeyInputToManager
                , createManager
                , changeFocus
                , focusOverridesEsc
                , unfocus
                ) where

import Graphics.X11 (KeyCode)
import XElement
import XContext
import XMenuGlobal
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (Reader, ask, liftIO)
import Data.Bool (bool)

data XEManager = XEManager { xem_elements   :: [XMElement]
                           , xem_inFocus    :: Maybe Int
                           }

createManager :: [XMenuGlobal -> XMElement] -> Reader XMenuGlobal XEManager
createManager xels = (\xmap -> return $ XEManager xmap Nothing)
                   . (\xmglobal -> (map (\x -> x xmglobal) xels) ) =<< ask

sendKeyInputToManager :: XEManager -> (KeyCode, String) -> IO XEManager
sendKeyInputToManager xem kdata =
            maybe (return xem)
                  (\foc -> do
                        nel <- sendKeyInput (xem_elements xem !! foc) kdata
                        let (p1, p2) = splitAt foc $ xem_elements xem
                        return $ xem { xem_elements = p1 ++ [nel]
                                                   ++ (bool (tail p2) []
                                                            (length p2 == 0)) }
                  )
                  (xem_inFocus xem)

drawAll :: XEManager -> XMContext -> ReaderT XMenuData IO ()
drawAll xem ctx = drawAll' (xem_elements xem) ctx
    where drawAll' (x:xs) ctx = do
            drawElement ctx x
            drawAll' xs ctx
          drawAll' [] _ = return ()

changeFocus :: XEManager -> XEManager
changeFocus xem = case xem_elements xem of
    []          -> xem
    otherwise   -> changeFocus' (xem_inFocus xem)
                                (maybe 0 (nextElement xem) (xem_inFocus xem))
                   xem
    where changeFocus' foc curr xem' =
                bool (changeFocus' foc (nextElement xem' curr) xem')
                     (bool (removeOldFocus foc . setNewFocus curr
                          $ xem' { xem_inFocus = Just curr } ) xem'
                          $ maybe False (curr ==) foc)
                    $ canFocus (xem_elements xem' !! curr) || case foc of
                        Just foc'   -> foc' == curr
                        Nothing     -> curr + 1 == length (xem_elements xem')
          nextElement xem' curr =
                    bool (curr + 1) 0 (curr + 1 == length (xem_elements xem'))
          removeOldFocus foc xem' = maybe xem' (\foc' ->
                        let (p1, p2) = splitAt foc' (xem_elements xem')
                            oldf = setFocus (xem_elements xem' !! foc') False
                        in xem' { xem_elements = p1 ++ [oldf]
                                              ++ (bool (tail p2) []
                                                       (length p2 == 0)) }) foc
          setNewFocus foc xem' = let (p1, p2) = splitAt foc (xem_elements xem')
                                     newf = setFocus (xem_elements xem' !! foc) True
                                 in xem' { xem_elements = p1 ++ [newf]
                                                       ++ (bool (tail p2) []
                                                          (length p2 == 0)) }

focusOverridesEsc :: XEManager -> Bool
focusOverridesEsc xem =
    maybe False (\foc' -> gp_overridesEsc
                        $ getGenProps (xem_elements xem !! foc'))
          (xem_inFocus xem)

unfocus :: XEManager -> XEManager
unfocus xem =
    maybe xem (\foc' -> let newf = setFocus (xem_elements xem !! foc') False
                            (p1, p2) = splitAt foc' (xem_elements xem)
                            newels = p1 ++ [newf] ++ (bool (tail p2) []
                                                    $ length p2 == 0)
                        in xem { xem_elements = newels
                               , xem_inFocus = Nothing
                               })
          (xem_inFocus xem)
