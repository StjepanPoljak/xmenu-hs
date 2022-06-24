module XManager ( XEManager(..)
                , drawAll
                , sendKeyInputToManager
                , createManager
                , changeFocus
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
createManager xels = (\xmap -> return $ XEManager xmap Nothing) . (\xmglobal -> (map (\x -> x xmglobal) xels) ) =<< ask

sendKeyInputToManager :: XEManager -> (KeyCode, String) -> IO XEManager
sendKeyInputToManager xem kdata =
            maybe (return xem)
                  (\foc -> do
                        nxel <- sendKeyInput (xem_elements xem !! foc) kdata
                        let (p1, p2) = splitAt foc $ xem_elements xem
                        return $ xem { xem_elements = (p1 ++ [nxel] ++ (bool (tail p2) [] (length p2 == 0))) }
                  )
                  (xem_inFocus xem)

drawAll :: XEManager -> XMContext -> ReaderT XMenuData IO ()
drawAll xem ctx = drawAll' (xem_elements xem) ctx
    where drawAll' (x:xs) ctx = do
            drawElement ctx x
            drawAll' xs ctx
          drawAll' [] _ = return ()

changeFocus :: XEManager -> XEManager
changeFocus xem@(XEManager [] _) = xem
-- changeFocus xem@(XEManager [x] _) = xem
changeFocus xem = changeFocus' (xem_inFocus xem)
                               (nextElement xem (maybe 0 id $ xem_inFocus xem)) xem
    where changeFocus' foc curr xem =
                bool (changeFocus' foc (nextElement xem curr) xem)
                     (xem { xem_inFocus = Just curr} )
                     (canFocus (xem_elements xem !! curr) || case foc of
                        Just foc'   -> foc' == curr
                        Nothing     -> curr == length (xem_elements xem))
          nextElement xem curr =
                    bool (curr + 1) 0 (curr + 1 == length (xem_elements xem))
