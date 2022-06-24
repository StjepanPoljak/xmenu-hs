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

--defaultLabelE :: Reader XMenuGlobal XMElement
defaultLabelE x y w h f = (\xmglobal -> XMLabelE $ f (runReader (defaultLabel x y w h) xmglobal))

class XMElementClass a where
    canFocus :: a -> Bool
    sendKeyInput :: a -> (KeyCode, String) -> IO a
    drawElement :: XMContext -> a -> ReaderT XMenuData IO ()
    onChange :: a -> IO ()

instance XMElementClass XMElement where
    canFocus (XMLabelE label)   = True
    canFocus (XMListE list)     = True
    sendKeyInput (XMLabelE label) (kc, str)
        | length str == 0   = return $ XMLabelE label
        | kc == 9           = return $ XMLabelE label
        | otherwise         = liftM XMLabelE $ (\xl -> do
                                                    (l_onChange label) xl
                                                    return xl)
                            . bool (appendCharToLabel label (head str))
                                   (removeCharFromLabel label) $ (kc == 22)
    sendKeyInput (XMListE list) (kc, str) = return $ XMListE list
    drawElement ctx (XMLabelE label) = drawLabel ctx label
    onChange (XMLabelE label) = l_onChange label $ label
    onChange (XMListE _) = return ()
