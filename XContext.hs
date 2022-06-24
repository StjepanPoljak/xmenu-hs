module XContext ( XMContext(..)
                , createContext
                ) where

import XMenuGlobal
import Graphics.X11 (Drawable, GC, Dimension, Position)
import Control.Monad.Reader (runReader, ask, Reader(..))

data XMContext = XMContext { c_drawable :: Drawable
                           , c_gc       :: GC
                           , c_width    :: Dimension
                           , c_height   :: Dimension
                           }

createContext :: Drawable -> GC -> Reader XMenuGlobal XMContext
createContext drawable gc = ask >>= \xmglob -> do
    let (XMenuGlobal xmopts _) = xmglob
    return $ XMContext drawable gc (g_width xmopts)
                       (g_height xmopts)
