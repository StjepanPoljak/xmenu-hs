module XElement ( XMElement(..)
                , XMElementClass(..)
                , defaultLabelE
                , emptyLabelE
                ) where

import XLabel
import XContext
import XMenuGlobal
import Graphics.X11 (KeyCode, setForeground, setBackground, fillRectangle, drawRectangle, Pixmap, Position)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Reader (runReader, liftIO)
import Control.Monad (liftM, when, foldM_)
import Data.Bool (bool)

data XMList a = XMList { li_gen       :: XMGenProps
                       , li_items     :: [a]
                       , li_selected  :: Maybe Int
                       , li_viewY     :: Position
                       }

data XMElement = XMLabelE XMLabel
               | XMListE (XMList XMElement)

processList :: (XMElementClass a) => XMList a -> XMList a
processList list = list { li_items = fst $ foldl (\(lst, lastY) item ->
    ( lst ++ [updateGenProps item
                     (\gp -> let h = fromIntegral $ gp_height (li_gen list)
                                 xP = fromIntegral $ gp_xPad (li_gen list)
                             in gp { gp_x = 0
                                   , gp_y = lastY + h
                                   , gp_width = gp_width (li_gen list) - 2 * xP
                                   }
                     )
             ]
    , lastY + fromIntegral (gp_width (getGenProps item))
    )) ([], fromIntegral (gp_yPad (li_gen list))) (li_items list) }

prepareListPixmap :: (XMElementClass a) => XMList a -> IO ()
prepareListPixmap list = foldM_ (\_ _ -> return "") "" (takeWhile (\item -> gp_y (getGenProps item) < li_viewY list) (li_items list))

drawList :: (XMElementClass a) => XMContext -> XMList a -> ReaderT XMenuData IO ()
drawList context list = ask >>= \xmdata -> liftIO $ do
    let display = g_display xmdata
    let lg = li_gen list
    let (fgColor, bgColor) = getColorsDynamic lg
    let (li_x, li_y) = (gp_x lg, gp_y lg)
    let (li_width, li_height) = (gp_width lg, gp_height lg)

    when (gp_background (li_gen list)) $ do
        setForeground display gc bgColor
        fillRectangle display drawable gc li_x li_y li_width li_height

    where XMContext drawable gc _ _ = context

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
    updateGenProps :: a -> (XMGenProps -> XMGenProps) -> a

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
    updateGenProps xmel f = setGenProps xmel $ f (getGenProps xmel)
