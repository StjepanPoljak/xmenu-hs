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

defaultLabelE name v x y w h f = XMLabelE
                               . f
                               . runReader (defaultLabel name v x y w h)

emptyLabelE name x y w h f = XMLabelE
                           . f
                           . runReader (emptyLabel name x y w h)

listLabelE name v h f = XMLabelE
                      . f
                      . runReader (listLabel name v h)

createListE name x y w h ih l f = XMListE
                                . f
                                . runReader (createList name x y w h ih l)

instance XMElementClass XMElement where

    sendKeyInput (XMLabelE label) = liftM XMLabelE
                                  . sendKeyInput label
    sendKeyInput (XMListE list) = liftM XMListE
                                . sendKeyInput list

    drawContents ctx (XMLabelE label) = drawContents ctx label
    drawContents ctx (XMListE list) = drawContents ctx list

    getGenProps (XMLabelE label) = getGenProps label
    getGenProps (XMListE list) = li_gen list

    setGenProps (XMListE list) = XMListE . setGenProps list
    setGenProps (XMLabelE label) = XMLabelE . setGenProps label

    getCallbacks _ = Nothing
