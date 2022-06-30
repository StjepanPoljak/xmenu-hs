module XElement ( XMElement(..)
                , defaultLabelE
                , emptyLabelE
                , listLabelE
                , createListE
                ) where

import XLabel
import XList
import XElementClass
import Control.Monad (liftM)
import Control.Monad.Trans.Reader (runReader, Reader)
import XMenuGlobal
import Graphics.X11 (Dimension)

data XMElement = XMLabelE XMLabel
               | XMListE (XMList XMElement)

defaultLabelE v x y w h f = XMLabelE .f . runReader (defaultLabel v x y w h)

emptyLabelE x y w h f = XMLabelE . f . runReader (emptyLabel x y w h)

listLabelE v h f = XMLabelE . f . runReader (listLabel v h)

createListE x y w h ih l f = XMListE . f . runReader (createList x y w h ih l)

instance XMElementClass XMElement where
    sendKeyInput (XMLabelE label) kdata = liftM XMLabelE
                                        $ sendKeyInput label kdata
    sendKeyInput (XMListE list) kdata = liftM XMListE
                                      $ sendKeyInput list kdata


--    sendKeyInput (XMListE list) (kc, str) = return $ XMListE list
    drawElement ctx (XMLabelE label) = drawElement ctx label
    drawElement ctx (XMListE list) = drawElement ctx list

    getGenProps (XMLabelE label) = getGenProps label
    getGenProps (XMListE list) = li_gen list
    setGenProps (XMListE list) gpr = XMListE $ list { li_gen = gpr }
    setGenProps (XMLabelE label) gpr = XMLabelE $ setGenProps label gpr

