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
import Control.Monad (unless, when, (<=<), liftM, foldM)
import XMenuGlobal
import XLabel
import XContext
import XManagerClass
import XElement
import XList
import System.Environment (getEnv)
import System.Directory (getDirectoryContents)
import Data.List (isPrefixOf, concat)

debug = True

isKeyEvent :: Event -> Bool
isKeyEvent (KeyEvent _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = True
isKeyEvent _ = False

getKeyCodeProperty :: Event -> (Event -> a) -> Maybe a
getKeyCodeProperty ev evf
    | isKeyEvent ev     = Just $ evf ev
    | otherwise         = Nothing

runReaderT' = flip runReaderT
runReader' = flip runReader

splitPathVar str = case dropWhile (== ':') str of
                        "" -> []
                        s' -> w : splitPathVar s''
                              where (w, s'') = break (==':') s'

getPathVar = liftM splitPathVar $ getEnv "PATH"

getFilesFromPathVar = foldM (\acc x -> liftM (filter (not
                                                   . (flip elem) [".", ".."])
                                            . concat
                                            . (:[acc]))
                                            $ getDirectoryContents x) []
                  =<< getPathVar

main =  do

    execList <- getFilesFromPathVar

    let xmopts = XMenuOpts 400 200 0x244758 0x12222a
                           (createFont "Terminus" 16)
                           10 5 15 15 0x6dcfff 0x12222a

    xmglob <- runReaderT createXMenu xmopts

    let (XMenuGlobal _ xmdata) = xmglob
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

        let context = createContext pixmap gc

        setForeground display gc (g_bgColor xmopts)
        fillRectangle display pixmap gc 0 0 (g_width xmopts)
                      (g_height xmopts)

        runReaderT' xmdata $ drawAll xman context

        copyArea display pixmap xmenuw gc 0 0 (g_width xmopts)
                 (g_height xmopts) 0 0

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

                when (debug) . putStrLn $ show (st, keyStr, x)

                loop =<< sendKeyInputToManager xman (x, keyStr)

            when (x==23) . loop . changeFocus xman $ Forward
            when (x==9 && focusOverridesEsc xman) . loop . unfocus $ xman

            ) $ getKeyCodeProperty ev ev_keycode

    let labelProps = (\lbl -> lbl { l_gen = (l_gen lbl)
                                            { gp_border = True } })

    let list = createListE 20 90 360 100 35
             . map (\str -> listLabelE str 0 labelProps)
             $ [ ("E1")
               , ("E2")
               , ("E3")
               , ("E4")
               , ("E5")
               , ("E6")
               , ("E7")
               ]

    let xman = runReader' xmglob
             $ createManager [ (emptyLabelE 20 20 360 50 $ \lbl -> lbl
                                           { l_gen = (l_gen lbl)
                                                     { gp_border = True
                                                     , gp_overridesEsc = True
                                                     }
                                           , l_onChange = \l -> print $ filter (isPrefixOf . l_val $ l) execList
                                           }
                               )
                             , (list $ \lst -> lst
                                           { li_gen = (li_gen lst)
                                                    { gp_overridesEsc = True
                                                    , gp_border = True }
                                           }
                               )
                             ]
    loop $ xman { xem_inFocus = Nothing }

    freeFont display fontstr

    putStrLn "Done."
