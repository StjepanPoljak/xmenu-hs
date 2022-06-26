module XElement ( XMElement(..)
                , XMElementClass(..)
                , defaultLabelE
                , emptyLabelE
                ) where

import XLabel
import XContext
import XMenuGlobal
import Graphics.X11 (KeyCode)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader (runReader)
import Control.Monad (liftM)
import Data.Bool (bool)

data XMList = XMList { li_gen       :: XMGenProps
                     , li_items     :: [XMElement]
                     , li_topItem   :: Int
                     }

data XMElement = XMLabelE XMLabel
               | XMListE XMList

-- drawList :: XMContext -> XMList -> ReaderT XMenuData IO ()
-- drawList context list = ask >>=

defaultLabelE v x y w h f = (\xmglobal -> XMLabelE
                          $ f (runReader (defaultLabel v x y w h) xmglobal))

emptyLabelE x y w h f = (\xmglobal -> XMLabelE
                      $ f (runReader (emptyLabel x y w h) xmglobal))

class XMElementClass a where
    canFocus :: a -> Bool
    sendKeyInput :: a -> (KeyCode, String) -> IO a
    drawElement :: XMContext -> a -> ReaderT XMenuData IO ()
    setFocus :: a -> Bool -> a
    getGenProps :: a -> XMGenProps
    setGenProps :: a -> XMGenProps -> a

instance XMElementClass XMElement where
    sendKeyInput (XMLabelE label) (kc, str)
        | length str == 0   = return $ XMLabelE label
        | otherwise         = liftM XMLabelE
                            $ either return (\lbl -> do
                                                l_onChange lbl $ lbl
                                                return lbl)
                            . bool (Right $ appendCharToLabel label (head str))
                                   (bool (Right $ removeCharFromLabel label)
                                         (Left label)
                                         (length (l_val label) == 0))
                            $ (kc == 22)
--    sendKeyInput (XMListE list) (kc, str) = return $ XMListE list
    drawElement ctx (XMLabelE label) = drawLabel ctx label

    canFocus = gp_canFocus . getGenProps
    setFocus xmel foc = setGenProps xmel (getGenProps xmel)
                                        { gp_focused = foc }
    getGenProps (XMLabelE label) = l_gen label
    getGenProps (XMListE list) = li_gen list
    setGenProps (XMLabelE label) gpr = XMLabelE $ label { l_gen = gpr }
    setGenProps (XMListE list) gpr = XMListE $ list { li_gen = gpr }
