module XContext ( XMContext(..)
                , createContext
                , drawBorder
                ) where

import XMenuGlobal
import Graphics.X11 (Drawable, GC, Dimension, Position, drawRectangle)
import Control.Monad.Reader (liftIO)
import Control.Monad.Trans.Reader (ask, ReaderT)
import Control.Monad (mapM_)

data XMContext = XMContext { c_drawable :: Drawable
                           , c_gc       :: GC
                           }

createContext :: Drawable -> GC -> XMContext
createContext drawable gc = XMContext drawable gc

drawBorder :: XMContext -> Position -> Position
           -> Dimension -> Dimension -> Dimension -> ReaderT XMenuData IO ()
drawBorder context x y w h bw = ask >>= \xmdata -> liftIO $ do
        let display = g_display xmdata
        let (drawable, gc) = (c_drawable context, c_gc context)
        mapM_ (\c -> drawRectangle display drawable gc
                                   (x + fromIntegral c) (y + fromIntegral c)
                                   (w - 2 * c - 2) (h - 2 * c - 2))
              [0..(bw - 1)]

