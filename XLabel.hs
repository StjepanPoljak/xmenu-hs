module XLabel
    ( XMLabel(..)
    , appendCharToLabel
    , removeCharFromLabel
    , defaultLabel
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

data XMLabel = XMLabel { l_val          :: String
                       , l_dispVal      :: Either String String
                       , l_x            :: Position
                       , l_y            :: Position
                       , l_width        :: Dimension
                       , l_height       :: Dimension
                       , l_xPad         :: Dimension
                       , l_yPad         :: Dimension
                       , l_fgColor      :: Pixel
                       , l_bgColor      :: Pixel
                       , l_border       :: Bool
                       , l_background   :: Bool
                       , l_fontStruct   :: FontStruct
                       , l_focused      :: Bool
                       , l_focColor     :: Pixel
                       , l_onChange     :: XMLabel -> IO ()
                       }

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

defaultLabel :: Position -> Position -> Dimension -> Dimension
             -> Reader XMenuGlobal XMLabel
defaultLabel x y w h = ask >>= \(XMenuGlobal xmopts xmdata) ->
            return $ XMLabel "" (Right "") x y w h
                             (g_xPad xmopts) (g_yPad xmopts)
                             (g_fgColor xmopts) (g_bgColor xmopts)
                             False False (g_fontStruct xmdata)
                             False (g_focColor xmopts) (\_ -> return ())

-- updateLabel :: XMContext -> XMLabel -> Reader XMenuData XMLabel
-- updateLabel context label = ask >>= \xmdata -> do
--    where lblw = bool (l_width label) c_w (l_width label == 0)
--          lblh = bool (l_height label) (fromIntegral asc + 2 * l_yPad label)
--                      (l_height label == 0)

drawLabel :: XMContext -> XMLabel -> RT.ReaderT XMenuData IO ()
drawLabel context label = RT.ask >>= \xmdata -> liftIO $ do
    let display = g_display xmdata
    when (l_background label) $ do
            setForeground display gc (l_bgColor label)
            fillRectangle display drawable gc (l_x label) (l_y label)
                          (l_width label) (l_height label)
    setForeground display gc (l_fgColor label)
    setBackground display gc (l_bgColor label)
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

