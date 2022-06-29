module XElement ( XMElement(..)
                , defaultLabelE
                , emptyLabelE
                ) where

import XLabel
import XList
import XElementClass
import Control.Monad (liftM)
import Control.Monad.Trans.Reader (runReader)

data XMElement = XMLabelE XMLabel
               | XMListE (XMList XMElement)

defaultLabelE v x y w h f = (\xmglobal -> XMLabelE
                          $ f (runReader (defaultLabel v x y w h) xmglobal))

emptyLabelE x y w h f = (\xmglobal -> XMLabelE
                      $ f (runReader (emptyLabel x y w h) xmglobal))

instance XMElementClass XMElement where
    sendKeyInput (XMLabelE label) (kc, str) = liftM XMLabelE $ sendKeyInput label (kc, str)

--    sendKeyInput (XMListE list) (kc, str) = return $ XMListE list
    drawElement ctx (XMLabelE label) = drawElement ctx label

    getGenProps (XMLabelE label) = getGenProps label
    getGenProps (XMListE list) = li_gen list
    setGenProps (XMListE list) gpr = XMListE $ list { li_gen = gpr }
    setGenProps (XMLabelE label) gpr = XMLabelE $ setGenProps label gpr

