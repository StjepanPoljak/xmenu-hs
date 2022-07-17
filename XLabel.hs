module XLabel
    ( XMLabel(l_gen,l_val,l_cbs)
    , defaultLabel
    , emptyLabel
    , listLabel
    ) where

import Graphics.X11
import XMenuGlobal
import qualified Control.Monad.Trans.Reader as RT (runReaderT, ReaderT, ask)
import Control.Monad.Reader (runReader, Reader, ask, liftIO)
import Control.Monad (when, unless, liftM, (<=<))
import Data.Bool (bool)
import Data.Either (fromRight, either)
import XContext
import XElementClass
import Data.Map (fromList, (!?))
import Data.List (singleton)

data XMLabel = XMLabel { l_gen          :: XMGenProps
                       , l_cbs          :: XMCallbacks XMLabel
                       , l_val          :: String
                       , l_dispVal      :: Either String String
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
l_fgFocColor    = gp_fgFocColor . l_gen
l_bgFocColor    = gp_bgFocColor . l_gen

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

getKeyStr str = maybe str id $ fromList specialChars !? str

allowedChars = (fst $ unzip specialChars) ++ alphanum
    where alphanum = map singleton $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

instance XMElementClass XMLabel where
    sendKeyInput label (kc, str) =

        (\lbl -> runCB2 lbl (kc, str) cb_onKeyPress) =<< case str of

            []  -> return label

            _   -> either return ((flip runCB) cb_onChange)
                 . bool (bool (Left label)
                              (Right . appendCharToLabel label
                                     . head . getKeyStr $ str)
                              (str `elem` allowedChars))
                        (case l_val label of
                              []  -> Left label
                              _   -> Right . removeCharFromLabel $ label)
                 $ kc == 22

    getGenProps = l_gen
    drawContents = drawLabel
    setGenProps label gpr = label { l_gen = gpr }
    getCallbacks = Just . l_cbs

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
    | null . l_val $ label  = label
    | otherwise             = case l_dispVal label of
        Right notFit    -> newLabel { l_dispVal = Right (l_val newLabel) }
        Left isFit      -> bool newLabel
                            (newLabel { l_dispVal = Right (l_val newLabel) })
                            (newWwPad < l_width label)
    where newLabel = label { l_val = bool (init . l_val $ label) ""
                                          (null . l_val $ label)
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

emptyDispVal :: Either String String
emptyDispVal = Right ""

emptyLabel :: String -> Position -> Position -> Dimension -> Dimension
           -> Reader XMenuGlobal XMLabel
emptyLabel name x y w h = defaultGenProps name x y w h >>= \gp ->
            return $ XMLabel gp defaultCallbacks "" emptyDispVal

defaultLabel :: String -> String -> Position -> Position -> Dimension
             -> Dimension -> Reader XMenuGlobal XMLabel
defaultLabel name v x y w h = emptyLabel name x y w h >>= \lbl ->
            return lbl { l_dispVal = getDispVal (lbl { l_val = v } ) }

listLabel :: String -> String -> Dimension -> Reader XMenuGlobal XMLabel
listLabel name v h = emptyLabel name 0 0 0 h >>= \lbl ->
            return lbl { l_val = v }

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

drawLabel :: XMContext -> XMLabel -> Dimension -> Dimension -> Bool
          -> RT.ReaderT XMenuData IO ()
drawLabel context label w h focd = RT.ask >>= \xmdata -> do
    let display = g_display xmdata
    let (fgColor, bgColor) = getColorsDynamic (l_gen label) focd
    let (drawable, gc) = (c_drawable context, c_gc context)

    when (either (const False)
                 ((not (null . l_val $ label) &&) . null)
               . l_dispVal $ label) $
        return =<< drawLabel context
                             (label { l_dispVal = getDispVal label }) w h focd

    liftIO $ do
        setForeground display gc fgColor
        setBackground display gc bgColor
        setFont display gc (fontFromFontStruct $ l_fontStruct label)
        drawImageString display drawable gc 0 (lbly) . either id id
                                                     $ (l_dispVal label)

    where lbly = fromIntegral $ (h + fromIntegral asc) `div` 2
          (_, asc, _, _) = textExtents (l_fontStruct label)
                                       (l_val label)
