module XElement ( XMElement(..)
                , XMElementClass(..)
                , defaultLabelE
                ) where

import XLabel
import XContext
import XMenuGlobal
import Graphics.X11 (KeyCode)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (runReader, Reader)
import Control.Monad ((>=>), (<=<), liftM)
import Data.Bool (bool)

data XMList = XMList Int

data XMElement = XMLabelE XMLabel
               | XMListE XMList

defaultLabelE x y w h f = (\xmglobal -> XMLabelE $ f (runReader (defaultLabel x y w h) xmglobal))

class XMElementClass a where
    canFocus :: a -> Bool
    sendKeyInput :: a -> (KeyCode, String) -> IO a
    drawElement :: XMContext -> a -> ReaderT XMenuData IO ()
    onChange :: a -> IO ()
    setFocus :: a -> Bool -> a

instance XMElementClass XMElement where
    canFocus (XMLabelE label)   = True
    canFocus (XMListE list)     = True
    sendKeyInput (XMLabelE label) (kc, str)
        | length str == 0   = return $ XMLabelE label
        | kc == 9           = return $ XMLabelE label
        | otherwise         = liftM XMLabelE
                            $ either return (\lbl -> do
                                                l_onChange lbl $ lbl
                                                return lbl)
                            . bool (Right $ appendCharToLabel label (head str))
                                   (bool (Right $ removeCharFromLabel label)
                                         (Left label)
                                         (length (l_val label) == 0))
                            $ (kc == 22)
    sendKeyInput (XMListE list) (kc, str) = return $ XMListE list
    drawElement ctx (XMLabelE label) = drawLabel ctx label
    onChange (XMLabelE label) = l_onChange label $ label
    onChange (XMListE _) = return ()
    setFocus (XMLabelE label) foc = XMLabelE $ label { l_focused = foc }
    setFocus (XMListE list) _ = XMListE list
