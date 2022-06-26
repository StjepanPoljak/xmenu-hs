module XLabel
    ( XMLabel(..)
    , appendCharToLabel
    , removeCharFromLabel
    , defaultLabel
    , emptyLabel
    , drawLabel
    ) where

import Graphics.X11
import XMenuGlobal
import qualified Control.Monad.Trans.Reader as RT (runReaderT, ReaderT, ask)
import Control.Monad.Reader (runReader, Reader, ask, liftIO)
import Control.Monad (when, unless, liftM)
import Data.Bool (bool)
import Data.Either (fromRight, either)
import XContext

data XMLabel = XMLabel { l_gen          :: XMGenProps
                       , l_val          :: String
                       , l_dispVal      :: Either String String
                       , l_onChange     :: XMLabel -> IO ()
                       }

l_x             = gp_x . l_gen
l_y             = gp_y . l_gen
l_width         = gp_width . l_gen
l_height        = gp_height . l_gen
l_xPad          = gp_xPad . l_gen
l_yPad          = gp_yPad . l_gen
l_fgColor       = gp_fgColor . l_gen
l_bgColor       = gp_bgColor . l_gen
l_border        = gp_border . l_gen
l_background    = gp_background . l_gen
l_fontStruct    = gp_fontStruct . l_gen
l_focused       = gp_focused . l_gen
l_fgFocColor    = gp_fgFocColor . l_gen
l_bgFocColor    = gp_bgFocColor . l_gen

appendCharToLabel :: XMLabel -> Char -> XMLabel
appendCharToLabel label char = case l_dispVal label of
        Right notFit    -> bool (newLabel {l_dispVal = Right (l_val newLabel)})
                                (fitText newLabel)
                                (newWwPad > l_width label)
        Left _          -> newLabel
    where newLabel = label { l_val = l_val label ++ [char] }
          newWidth = fromIntegral $ textWidth (l_fontStruct label)
                                              (l_val newLabel)
          newWwPad = newWidth + 2 * (l_xPad label)

removeCharFromLabel :: XMLabel -> XMLabel
removeCharFromLabel label
    | length (l_val label) == 0 = label
    | otherwise                 = case l_dispVal label of
        Right notFit    -> newLabel { l_dispVal = Right (l_val newLabel) }
        Left isFit      -> bool newLabel
                            (newLabel { l_dispVal = Right (l_val newLabel) })
                            (newWwPad < l_width label)
    where newLabel = label { l_val = bool (init $ l_val label) ""
                                          ((length $ l_val label) == 0)
                           }
          newWidth = fromIntegral $ textWidth (l_fontStruct label)
                                              (l_val newLabel)
          newWwPad = newWidth + 2 * (l_xPad label)

fitText :: XMLabel -> XMLabel
fitText label
    | dispLen == 0                  = label
    | widthWithPad > l_width label  = fitText (label {
                                        l_dispVal = liftM init (l_dispVal label)
                                        })
    | otherwise                     = label { l_dispVal = Left currLabel }
    where width' = fromIntegral $ textWidth (l_fontStruct label) currLabel
          widthWithPad = width' + (2 * l_xPad label)
          currLabel = (fromRight "" $ l_dispVal label) ++ "..."
          dispLen = length (either id id (l_dispVal label))

-- TotalChars :: CurrChars = TotalWidth : CurrWidth
-- CurrChars * TotalWidth = TotalChars * CurrWidth
-- CurrChars = TotalChars * CurrWidth / TotalWidth

emptyLabel :: Position -> Position -> Dimension -> Dimension
           -> Reader XMenuGlobal XMLabel
emptyLabel x y w h = ask >>= \(XMenuGlobal xmopts xmdata) ->
            return $ XMLabel (XMGenProps x y w h
                             (g_xPad xmopts) (g_yPad xmopts)
                             (g_fgColor xmopts) (g_bgColor xmopts) False
                             False (g_fontStruct xmdata) False True False
                             (g_fgFocColor xmopts) (g_bgFocColor xmopts)
                             ) "" (Right "") (\_ -> return ())

defaultLabel :: String -> Position -> Position -> Dimension -> Dimension
             -> Reader XMenuGlobal XMLabel
defaultLabel v x y w h = do
            lbl <- emptyLabel x y w h
            let lbl' = lbl { l_val = v }
            return $ lbl' { l_dispVal = getDispVal lbl' }

-- updateLabel :: XMContext -> XMLabel -> Reader XMenuData XMLabel
-- updateLabel context label = ask >>= \xmdata -> do
--    where lblw = bool (l_width label) c_w (l_width label == 0)
--          lblh = bool (l_height label) (fromIntegral asc + 2 * l_yPad label)
--                      (l_height label == 0)

l_width' label = l_width label - 2 * (l_xPad label)

getDispVal :: XMLabel -> Either String String
getDispVal label
    | totalWidth < l_width' label       = Right $ l_val label
    | approxCharWidth < l_width' label  = Left $ addUntilFit approxCharCount
    | otherwise                         = Left $ delUntilFit approxCharCount
    where approxDispVal = take approxCharCount (l_val label)
          totalWidth = fromIntegral $ textWidth (l_fontStruct label)
                                                (l_val label)
          approxCharCount = fromIntegral $ (fromIntegral (length (l_val label))
                          * fromIntegral (l_width' label)) `div` totalWidth
          approxCharWidth = fromIntegral $ textWidth (l_fontStruct label)
                                                     (approxDispVal ++ "...")
          addUntilFit cc =
            let dv = take cc (l_val label)
            in bool (dv ++ "...") (addUntilFit (cc + 1))
                    (cc < (length $ l_val label)
                       && (fromIntegral (textWidth (l_fontStruct label)
                                                   (dv ++ "..."))
                        < fromIntegral (l_width' label)))
          delUntilFit cc =
            let dv = take cc (l_val label)
            in bool (dv ++ "...") (delUntilFit (cc - 1))
                    (cc > 0 && (fromIntegral (textWidth (l_fontStruct label)
                                                        (dv ++ "..."))
                        > fromIntegral (l_width' label)))

drawLabel :: XMContext -> XMLabel -> RT.ReaderT XMenuData IO ()
drawLabel context label = RT.ask >>= \xmdata -> liftIO $ do
    let display = g_display xmdata
    let bgColor = bool (l_bgColor label) (l_bgFocColor label) (l_focused label)
    let fgColor = bool (l_fgColor label) (l_fgFocColor label) (l_focused label)
    when (l_background label) $ do
            setForeground display gc bgColor
            fillRectangle display drawable gc (l_x label) (l_y label)
                          (l_width label) (l_height label)
    setForeground display gc fgColor
    setBackground display gc bgColor
    setFont display gc (fontFromFontStruct $ l_fontStruct label)
    when (l_border label) $ drawRectangle display drawable gc
                                        (l_x label) (l_y label)
                                        (l_width label) (l_height label)
    drawImageString display drawable gc lblx (l_y label + lbly)
                    (either id id (l_dispVal label))
    where lbly = fromIntegral $ (l_height label + fromIntegral asc) `div` 2
          (_, asc, _, _) = textExtents (l_fontStruct label) (l_val label)
          lblx = fromIntegral (l_xPad label) + l_x label
          XMContext drawable gc c_w c_h = context

