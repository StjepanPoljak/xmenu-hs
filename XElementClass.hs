module XElementClass ( XMElementClass(..)
                     ) where

import XContext
import XMenuGlobal
import Graphics.X11 (KeyCode)
import Control.Monad.Trans.Reader (ReaderT)

class XMElementClass a where
    sendKeyInput :: a -> (KeyCode, String) -> IO a
    drawElement :: XMContext -> a -> ReaderT XMenuData IO ()
    getGenProps :: a -> XMGenProps
    setGenProps :: a -> XMGenProps -> a

    canFocus :: a -> Bool
    canFocus = gp_canFocus . getGenProps

    setFocus :: a -> Bool -> a
    setFocus xmel foc = setGenProps xmel (getGenProps xmel)
                                        { gp_focused = foc }

    updateGenProps :: a -> (XMGenProps -> XMGenProps) -> a
    updateGenProps xmel f = setGenProps xmel $ f (getGenProps xmel)

