module XLabel
    ( XMLabel(l_gen, l_val, l_events, l_mode)
    , XMLabelMode(tf_cursorPeek, tf_background, tf_cursorHide)
    , defaultLabel
    , emptyLabel
    , emptyTextField
    , listLabel
    , drawLabel
    ) where

import Graphics.X11

import Control.Monad.Reader (runReader, Reader, ask, liftIO)
import Control.Monad (when, unless, liftM, (<=<))
import qualified Control.Monad.Trans.Reader as RT (runReaderT, ReaderT, ask)

import Data.Bool (bool)
import Data.Either (fromRight, either)
import Data.Function ((&))
import Data.Maybe (fromJust)

import qualified Data.Map as M (fromList, (!?))

import XContext
import XElementClass
import XMenuGlobal
import XEvent

data XMLabelMode = XMLabelM
                 | XMTextFieldM { tf_cursor     :: Int
                                , tf_cursorW    :: Dimension
                                , tf_cursorX    :: Position
                                , tf_cursorPeek :: Dimension
                                , tf_cursorHide :: Bool
                                , tf_viewX      :: Position
                                , tf_background :: Maybe Pixmap }

data XMLabel = XMLabel { l_gen          :: XMGenProps
                       , l_events       :: XMElEventMap XMLabel
                       , l_val          :: String
                       , l_dispVal      :: Either String String
                       , l_mode         :: XMLabelMode
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

specialChars = [ (xK_space,         ' ')
               , (xK_comma,         ',')
               , (xK_period,        '.')
               , (xK_underscore,    '_')
               , (xK_minus,         '-')
               , (xK_colon,         ':')
               , (xK_semicolon,     ';')
               , (xK_quotedbl,      '\"')
               , (xK_apostrophe,    '\'')
               , (xK_ampersand,     '&')
               , (xK_exclam,        '!')
               , (xK_parenleft,     '(')
               , (xK_parenright,    ')')
               , (xK_slash,         '/')
               ]
specialCharsMap = M.fromList specialChars

getKeyStr :: KeySym -> Char
getKeyStr ks = maybe (head . keysymToString $ ks) id
             . (M.!?) specialCharsMap $ ks

allowedChars = (fst $ unzip specialChars) ++ alphanum
    where alphanum = map (stringToKeysym . \x -> [x])
                   $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

instance XMElementClass XMLabel where
    sendKeyInput lbl = case l_mode lbl of
                    XMLabelM                    -> sendKeyInputLabel lbl
                    XMTextFieldM _ _ _ _ _ _ _  -> sendKeyInputTextField lbl
    getGenProps = l_gen
    drawContents ctx lbl = case l_mode lbl of
                    XMLabelM                    -> drawLabel ctx lbl
                    XMTextFieldM _ _ _ _ _ _ _  -> drawTextField ctx lbl

    setGenProps label gpr = label { l_gen = gpr }
    getElEventMap = l_events

sendKeyInputLabel label ks =
    (\(lbl, rdrw) -> liftM ((flip (,)) rdrw)
                   $ runElEvent lbl (XMKeyEvent ks))
         <=< either (\lbl -> return (lbl, False))
                    (\lbl -> liftM ((flip (,)) True)
                           $ runElEvent lbl XMChangeEvent)
           $ if ks == xK_BackSpace
             then bool (Right $ removeCharFromLabel label)
                       (Left label)
                       (null $ l_val label)

             else bool (Left label)
                       (Right . appendCharToLabel label
                              . getKeyStr $ ks)
                       (ks `elem` allowedChars)

moveCursor :: KeySym -> XMLabel -> XMLabel
moveCursor ks lbl = alignAfterMove . updateCursorX
                  $ lbl { l_mode = (l_mode lbl)
                            { tf_cursor = maybe l_cursor id
                                        . (M.!?) ksMap $ ks
                            }
                        }
    where l_cursor = tf_cursor (l_mode lbl)
          ksMap = M.fromList [ ( xK_Left
                               , max 0 (l_cursor - 1)
                               )
                             , ( xK_Right
                               , min (length $ l_val lbl)
                                     (l_cursor + 1))
                             , ( xK_Home
                               , 0
                               )
                             , ( xK_End
                               , length $ l_val lbl
                               )
                             ]

sendKeyInputTextField label ks =
    (\(lbl, rdrw) -> liftM ((flip (,)) rdrw)
                   $ runElEvent lbl (XMKeyEvent ks))
         <=< (\(changed, redraw, lbl) ->
                    bool (return (lbl, redraw))
                         (liftM ((flip (,)) True) . runElEvent lbl
                                                  $ XMChangeEvent)
                         changed)
           $ if ks `elem` [xK_BackSpace, xK_Delete]
             then let newL = removeCharFromTextField ks label
                  in ( l_val label /= l_val newL
                     , True
                     , newL
                     )
             else if ks `elem` [xK_Left, xK_Right, xK_Home, xK_End]
             then let newL = moveCursor ks label
                  in ( False
                     , tf_cursor (l_mode label) /= tf_cursor (l_mode newL)
                     , newL
                     )
             else bool (( False
                        , False
                        , label
                        ))
                       (( True
                        , True
                        , addCharToTextField label . getKeyStr $ ks
                       ))
                       (ks `elem` allowedChars)

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

removeCharFromTextField :: KeySym -> XMLabel -> XMLabel
removeCharFromTextField ks label
    | l_cursor == 0 && ks == xK_BackSpace                   = label
    | l_cursor == length (l_val label) && ks == xK_Delete   = label
    | otherwise = adjustAfterModify XMRem
                . updateCursorX
                $ label { l_val = remove (l_val label)
                                . (l_cursor +)
                                . fromJust
                                . (flip (M.!?)) ks
                                $ ksMap

                        , l_mode = (l_mode label)
                                { tf_cursor = (l_cursor +)
                                            . fromJust
                                            . (flip (M.!?)) ks
                                            $ ksMap }
                        }
    where remove str pos = take pos str ++ drop (pos + 1) str
          l_cursor = tf_cursor (l_mode label)
          ksMap = M.fromList $ [ (xK_BackSpace, -1)
                               , (xK_Delete, 0)
                               ]

addCharToTextField :: XMLabel -> Char -> XMLabel
addCharToTextField label char = adjustAfterModify XMAdd
                                 . updateCursorX
                                 $ label { l_val   = newVal
                                         , l_mode  = (l_mode label)
                                            { tf_cursor = l_cursor + 1 }
                                         }
    where insert str ch pos
            | pos == 0              = ch:str
            | pos == length str     = str ++ [ch]
            | otherwise             = take pos str ++ [ch] ++ drop pos str
          l_cursor = tf_cursor (l_mode label)
          newVal = insert (l_val label) char (l_cursor)

--           viewX
--           |--tW - viewX---|
-- |-------------tW----------|
-- |there is |some text| here|
--           |some t|xt|
--           |----vW---|
--                  |
--                  curX

labelViewWidth :: XMLabel -> Dimension
labelViewWidth lbl = gp_width (l_gen lbl)
                   - fromIntegral (tf_cursorW $ l_mode lbl)
                   - (2 * gp_xPad (l_gen lbl))

labelTextWidth :: XMLabel -> Dimension
labelTextWidth lbl = fromIntegral $ textWidth (l_fontStruct lbl) (l_val lbl)

getRelativeCursorX :: XMLabel -> Position
getRelativeCursorX lbl = tf_cursorX (l_mode lbl) - tf_viewX (l_mode lbl)

data XMModifyMode = XMAdd | XMRem deriving Eq

adjustAfterModify :: XMModifyMode -> XMLabel -> XMLabel
adjustAfterModify mode lbl
    | cursorOut = lbl { l_mode = (l_mode lbl)
                        { tf_viewX = case mode of
                            XMAdd   -> let newX = l_cursorX
                                                - fromIntegral lblVW
                                           csum = newX + cursorPeek
                                       in bool (fromIntegral $ lblTW - lblVW)
                                               csum
                                        . (<) csum
                                        . fromIntegral
                                        $ remW

                            XMRem   -> let diff = l_cursorX - cursorPeek
                                       in bool 0 diff . (>) diff $ 0
                        }
                      }

    | otherwise = if tf_viewX (l_mode lbl) < 0
                  then lbl { l_mode = (l_mode lbl) { tf_viewX = 0 } }

                  else case mode of

                    XMAdd   -> lbl

                    XMRem   -> if lblTW > lblVW && remW < 0
                               then lbl { l_mode = (l_mode lbl)
                                            { tf_viewX = fromIntegral
                                                       $ lblTW - lblVW
                                            }
                                        }
                               else if lblTW < lblVW && l_cursorX /= 0
                               then lbl { l_mode = (l_mode lbl)
                                            { tf_viewX = 0 }
                                        }
                               else lbl

    where lblVW = labelViewWidth lbl
          lblTW = labelTextWidth lbl
          l_cursorX = tf_cursorX $ l_mode lbl
          remW = fromIntegral lblTW - fromIntegral lblVW
               - fromIntegral (tf_viewX $ l_mode lbl)
          cursorOut = let rcx = getRelativeCursorX lbl
                      in rcx < 0 || rcx > fromIntegral lblVW
          cursorPeek = (fromIntegral . tf_cursorPeek $ l_mode lbl) :: Position


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
            return $ XMLabel gp emptyEventMap "" emptyDispVal XMLabelM

emptyTextField :: String -> Position -> Position -> Dimension -> Dimension
               -> Reader XMenuGlobal XMLabel
emptyTextField name x y w h = defaultGenProps name x y w h >>= \gp ->
            return $ XMLabel gp emptyEventMap "" emptyDispVal
                             (XMTextFieldM 0 2 0 16 False 0 Nothing)

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

getCursorX :: XMLabel -> Position
getCursorX label = textWidth (l_fontStruct label)
                 . take cursorCount
                 . l_val $ label

    where cursorCount = tf_cursor . l_mode $ label

updateCursorX :: XMLabel -> XMLabel
updateCursorX lbl = lbl { l_mode = (l_mode lbl)
                                { tf_cursorX = getCursorX lbl }
                        }

alignAfterMove :: XMLabel -> XMLabel
alignAfterMove lbl
    | l_cursorX < l_viewX
                + fromIntegral l_cursorW = lbl { l_mode = (l_mode lbl)
                                                { tf_viewX = l_cursorX }
                                               }
    | l_cursorX + fromIntegral l_cursorW
    > l_viewX + fromIntegral l_width = lbl { l_mode = (l_mode lbl)
                                            { tf_viewX = l_cursorX
                                                       - fromIntegral (l_width
                                                                - l_cursorW) }
                                           }
    | otherwise = lbl

    where l_cursorX = tf_cursorX $ l_mode lbl
          l_viewX = tf_viewX $ l_mode lbl
          l_cursorW = tf_cursorW $ l_mode lbl
          l_width = gp_width (l_gen lbl)
                  - fromIntegral (2 * gp_xPad (l_gen lbl))

drawTextField :: XMContext -> XMLabel -> Dimension -> Dimension -> Bool
              -> RT.ReaderT XMenuData IO ()
drawTextField context textf w h False = drawLabel context textf w h False
drawTextField context textf w h True = RT.ask >>= \xmdata -> do
    let display = g_display xmdata
    let (fgColor, bgColor) = getColorsDynamic (l_gen textf) True
    let (drawable, gc) = (c_drawable context, c_gc context)
    let el_gp = getGenProps textf
    let (el_w, el_h) = (gp_width el_gp, gp_height el_gp)
    let pw = fromIntegral $ textWidth (l_fontStruct textf) (l_val textf)
                          + fromIntegral (tf_cursorW $ l_mode textf) + 1

    liftIO $ do
        pixmap <- createPixmap display drawable
                                  pw (l_height textf)
                $ defaultDepthOfScreen
                . defaultScreenOfDisplay
                $ display

        setForeground display gc bgColor
        fillRectangle display pixmap gc 0 0 pw el_h

        setForeground display gc fgColor
        setBackground display gc bgColor
        setFont display gc (fontFromFontStruct $ l_fontStruct textf)
        drawString display pixmap gc 1 lbly (l_val textf)

        when (not . tf_cursorHide $ l_mode textf) $
            fillRectangle display pixmap gc (tf_cursorX $ l_mode textf)
                          (lbly - (fromIntegral asc') + 4)
                          (tf_cursorW $ l_mode textf) (fromIntegral asc')

        copyArea display pixmap drawable gc (tf_viewX $ l_mode textf) 0
                 pw el_h 0 0

        freePixmap display pixmap

    where lbly = fromIntegral $ (h + fromIntegral asc) `div` 2
          (_, asc, _, _) = textExtents (l_fontStruct textf)
                                       (l_val textf)
          asc' = asc + 8
