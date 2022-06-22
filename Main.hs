module Main where

import XWindow

import Graphics.X11
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Control.Concurrent (threadDelay)
import Data.Word (Word32)
import Data.Bits ((.|.))
import Data.Tuple (swap)
import System.IO
import Control.Monad (unless, when, liftM, guard, mzero)
import Control.Monad.Reader (runReader, Reader(..), ask, liftIO)
import qualified Control.Monad.Trans.Reader as RT (runReaderT, ReaderT(..), ask)
import Data.Maybe (maybe)
import Data.Either (fromRight, either)
import Data.Bool (bool)
import qualified Data.Map as Map
import XMenuGlobal

isKeyEvent :: Event -> Bool
isKeyEvent (KeyEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = True
isKeyEvent _ = False

getKeyCodeProperty :: Event -> (Event -> a) -> Maybe a
getKeyCodeProperty ev evf
    | isKeyEvent ev     = Just $ evf ev
    | otherwise         = Nothing

specialChars = [ ("space",      " ")
               , ("comma",      ",")
               , ("period",     ".")
               , ("underscore", "_")
               , ("minus",      "-")
               , ("colon",      ":")
               , ("semicolon",  ";")
               , ("quotedbl",   "\"")
               , ("ampersand",  "&")
               , ("exclam",     "!")
               , ("parenleft",  "(")
               , ("parenright", ")")
               ]

allowedChars = (fst $ unzip specialChars) ++ alphanum
    where alphanum = map (\ch -> [ch]) $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

getKeyStr str = maybe str id $ Map.fromList specialChars Map.!? str

data XMLabel = XMLabel { val        :: String
                       , dispVal    :: Either String String
                       , x          :: Position
                       , y          :: Position
                       , width      :: Dimension
                       , height     :: Dimension
                       , xPad       :: Dimension
                       , yPad       :: Dimension
                       , fgColor    :: Pixel
                       , bgColor    :: Pixel
                       , border     :: Bool
                       , background :: Bool
                       , fontStruct :: FontStruct
                       }

appendCharToLabel :: XMLabel -> Char -> XMLabel
appendCharToLabel label char = case dispVal label of
        Right notFit    -> bool (newLabel {dispVal = Right (val newLabel)})
                                (fitText newLabel)
                                (newWwPad > width label)
        Left _          -> newLabel
    where newLabel = label { val = val label ++ [char] }
          newWidth = fromIntegral $ textWidth (fontStruct label) (val newLabel)
          newWwPad = newWidth + 2 * (xPad label)

removeCharFromLabel :: XMLabel -> XMLabel
removeCharFromLabel label = case dispVal label of
        Right notFit    -> newLabel { dispVal = Right (val newLabel) }
        Left isFit      -> bool newLabel
                                (newLabel { dispVal = Right (val newLabel) })
                                (newWwPad < width label)
    where newLabel = label { val = bool (init $ val label) ""
                                        ((length $ val label) == 0)
                           }
          newWidth = fromIntegral $ textWidth (fontStruct label) (val newLabel)
          newWwPad = newWidth + 2 * (xPad label)

fitText :: XMLabel -> XMLabel
fitText label
    | widthWithPad > width label    = fitText (label {
                                          dispVal = liftM init (dispVal label)
                                        })
    | otherwise                     = label { dispVal = Left currLabel }
    where width' = fromIntegral $ textWidth (fontStruct label) currLabel
          widthWithPad = width' + (2 * xPad label)
          currLabel = (fromRight "" $ dispVal label) ++ "..."

defaultLabel :: Position -> Position -> Dimension -> Dimension
             -> Reader XMenuGlobal XMLabel
defaultLabel x y w h = ask >>= \(XMenuGlobal xmopts xmdata) ->
            return $ XMLabel "" (Right "") x y w h
                             (g_xPad xmopts) (g_yPad xmopts)
                             (g_fgColor xmopts) (g_bgColor xmopts)
                             False False (g_fontStruct xmdata)

createLabel :: Drawable -> GC -> XMLabel -> RT.ReaderT XMenuData IO ()
createLabel drawable gc label = RT.ask >>= \xmdata -> liftIO $ do
    let display = g_display xmdata
    when (background label) $ do
            setForeground display gc (bgColor label)
            fillRectangle display drawable gc (x label) (y label)
                          (width label) (height label)
    setForeground display gc (fgColor label)
    setBackground display gc (bgColor label)
    setFont display gc (fontFromFontStruct $ fontStruct label)
    when (border label) $ drawRectangle display drawable gc (x label) (y label)
                                        (width label) (height label)
    drawImageString display drawable gc lblx (y label + lbly)
                    (either id id (dispVal label))
    where lbly = fromIntegral $ ((height label) + (fromIntegral asc)) `div` 2
          (_, asc, _, _) = textExtents (fontStruct label) (val label)
          lblx = fromIntegral (xPad label) + x label

main =  do

    let xmopts = XMenuOpts 400 200 0x12222a 0x6dcfff
                           "-*-Terminus-*-*-*-*-16-*-*-*-*-*-*-*" 20 10

    xmglobal <- RT.runReaderT createXMenu xmopts

    let (XMenuGlobal _ xmdata) = xmglobal
    let (XMenuData display _ _ fontstr xmenuw) = xmdata

    mapWindow display xmenuw
    clearWindow display xmenuw
    selectInput display xmenuw
                (exposureMask .|. keyPressMask .|. buttonPressMask)
    setInputFocus display xmenuw revertToPointerRoot 0

    let txtBox = ((flip runReader) xmglobal $ defaultLabel 20 20 100 40)
                 { border = True }

    let loop label = do
        gc <- createGC display xmenuw

        pixmap <- createPixmap display xmenuw
                               (g_width xmopts) (g_height xmopts)
                               $ defaultDepthOfScreen
                               . defaultScreenOfDisplay
                               $ display

        setForeground display gc (g_bgColor xmopts)
        fillRectangle display pixmap gc 0 0 (g_width xmopts) (g_height xmopts)

        (flip RT.runReaderT) xmdata $ createLabel pixmap gc label

        copyArea display pixmap xmenuw gc 0 0 400 200 0 0

        freeGC display gc
        freePixmap display pixmap
        sync display True

        ev <- allocaXEvent $ \xptr -> do
            nextEvent display xptr
            event <- getEvent xptr
            return event

        maybe (loop label) (\x -> case x of
            9   -> return ()

            22  -> loop (removeCharFromLabel label)

            _   -> do
                let (Just st)   = getKeyCodeProperty ev ev_state
                sym             <- keycodeToKeysym display x (fromIntegral st)
                let keyStr      = keysymToString sym

                case keyStr `elem` allowedChars of
                    True    -> loop $ appendCharToLabel label
                                            (head $ getKeyStr keyStr)

                    _       -> loop label

            ) $ getKeyCodeProperty ev ev_keycode

    loop txtBox

    freeFont display fontstr

    putStrLn "DONE"

