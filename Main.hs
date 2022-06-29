module Main where

import XWindow

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (Event(..), getEvent, ev_state, ev_keycode)

import Control.Concurrent (threadDelay)
import Data.Bits ((.|.))
import Control.Monad.Reader (runReader)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Maybe (maybe)
import Data.Bool (bool)
import Control.Monad (unless, when)
import qualified Data.Map as Map
import XMenuGlobal
import XLabel
import XContext
import XManager
import XElement

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

main =  do

    let xmopts = XMenuOpts 400 200 0x244758 0x12222a
                           (createFont "Terminus" 16)
                           20 10 15 15 0x6dcfff 0x12222a

    xmglobal <- runReaderT createXMenu xmopts

    let (XMenuGlobal _ xmdata) = xmglobal
    let (XMenuData display _ _ fontstr xmenuw) = xmdata

    mapWindow display xmenuw
    clearWindow display xmenuw
    selectInput display xmenuw
                (exposureMask .|. keyPressMask .|. buttonPressMask)
    setInputFocus display xmenuw revertToPointerRoot 0

    let loop xman = do
        gc <- createGC display xmenuw

        pixmap <- createPixmap display xmenuw
                               (g_width xmopts) (g_height xmopts)
                               $ defaultDepthOfScreen
                               . defaultScreenOfDisplay
                               $ display

        let context = (flip runReader) xmglobal $ createContext pixmap gc

        setForeground display gc (g_bgColor xmopts)
        fillRectangle display pixmap gc 0 0 (c_width context)
                      (c_height context)
        putStrLn $ show $ getFocus xman
        (flip runReaderT) xmdata $ drawAll xman context

        copyArea display pixmap xmenuw gc 0 0 (c_width context)
                 (c_height context) 0 0

        freeGC display gc
        freePixmap display pixmap
        sync display True

        ev <- allocaXEvent $ \xptr -> do
            nextEvent display xptr
            event <- getEvent xptr
            return event

        maybe (loop xman) (\x -> do
            unless (x == 9 || x == 23) $ do
                let (Just st)   = getKeyCodeProperty ev ev_state
                sym             <- keycodeToKeysym display x (fromIntegral st)
                let keyStr      = keysymToString sym
                putStrLn $ show (st, keyStr, x)

                loop =<< bool (return xman)
                              (sendKeyInputToManager xman
                                                     (x, getKeyStr keyStr))
                              (keyStr `elem` allowedChars || x == 22)
            when (x==23) (loop $ changeFocus xman)
            when (x==9 && focusOverridesEsc xman) (loop $ unfocus xman)

            ) $ getKeyCodeProperty ev ev_keycode

    let xman = (flip runReader) xmglobal
             $ createManager [ (emptyLabelE 20 20 360 50 (\lbl -> lbl
                                           { l_gen = (l_gen lbl)
                                                     { gp_border = True
                                                     , gp_overridesEsc = True
                                                     }
                                           , l_onChange = (\xl ->
                                                putStrLn $ (l_val xl) ++ " : " ++ (show (l_dispVal xl))
                                                ) })
                               )
                             , (defaultLabelE "Stjepan Poljak je najbolji"
                                              20 90 100 50 (\lbl -> lbl
                                              { l_gen = (l_gen lbl)
                                                        { gp_border = True
                                                        , gp_overridesEsc = True
                                                        }
                                              , l_onChange = (\xl -> do
                                                putStrLn $ (l_val xl) ++ " : " ++ (show (l_dispVal xl)) )})
                               )
                             ]
    loop $ xman { xem_inFocus = Nothing }

    freeFont display fontstr

    putStrLn "Done."
