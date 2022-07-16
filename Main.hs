module Main where

import XWindow

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (Event(..), getEvent, ev_state, ev_keycode, setEventType, eventName, setClientMessageEvent')
import System.Process
import Control.Concurrent (threadDelay)
import Data.Bits ((.|.))
import Control.Monad.Reader (runReader, liftIO)
import Control.Monad.Trans.Reader (runReaderT, ReaderT, ask)
import Data.Maybe (maybe)
import Data.Bool (bool)
import Control.Monad (unless, when, (<=<), liftM, foldM, void)
import XMenuGlobal
import XLabel
import XContext
import XManagerClass
import XElementClass
import XElement
import XList
import System.Environment (getEnv)
import System.Directory (getDirectoryContents)
import Data.List (isPrefixOf, concat, sort)
import Data.Function ((&))

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TBQueue
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (atomically)

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

sendXMEvent :: (XEManagerClass a, XMElementClass b)
            => TBQueue (a b -> IO (a b)) -> (a b -> IO (a b))
            -> ReaderT XMenuData IO ()
sendXMEvent tbq cb = ask >>= \xmdata -> liftIO $ do
    atomically . writeTBQueue tbq $ cb
    xmonad_test <- internAtom (g_display xmdata) "XMONAD_TEST" False
    allocaXEvent $ \ev -> do
        setEventType ev clientMessage
        setClientMessageEvent' ev (g_xmenuw xmdata) xmonad_test 32 []
        sendEvent (g_display xmdata) (g_xmenuw xmdata) False (structureNotifyMask) ev
    (flip sync) False . g_display $ xmdata

runXMEvents :: (XEManagerClass a, XMElementClass b) => a b
            -> TBQueue (a b -> IO (a b)) -> IO (a b)
runXMEvents xman tbq = maybe (return xman)
                             (\ev -> return
                                 =<< (flip runXMEvents) tbq
                                 =<< ev xman)
                   <=< atomically
                     . tryReadTBQueue
                     $ tbq

main = do

    execList <- getFilesFromPathVar

    eventQueue <- newTBQueueIO 128

    let xmopts = XMenuOpts 400 200 0x244758 0x12222a
                           (createFont "Terminus" 16)
                           10 5 15 15 0x6dcfff 0x12222a

    xmglob <- runReaderT createXMenu xmopts

    let (XMenuGlobal _ xmdata) = xmglob
    let (XMenuData display _ _ fontstr xmenuw) = xmdata

    selectInput display xmenuw
                (exposureMask .|. keyPressMask .|. buttonPressMask .|. structureNotifyMask)
    mapWindow display xmenuw
    setInputFocus display xmenuw revertToParent 0

    let loop xman = do

        ev <- allocaXEvent $ \xptr -> do
            nextEvent display xptr
            event <- getEvent xptr
            return event

        case ev of

            ClientMessageEvent _ _ _ _ _ _ _ -> do

                loop xman

            KeyEvent _ _ _ _ _ _ _ _ _ _ _ _ st kc _ -> do

                unless (kc == 9 || kc == 23) $ do
                    sym             <- keycodeToKeysym display kc
                                     . fromIntegral $ st
                    let keyStr      = keysymToString sym

                    when (debug) . putStrLn $ show (kc, keyStr, st)

                    loop =<< sendKeyInputToManager xman (kc, keyStr)

                when (kc == 23) . loop
                                . changeFocus xman
                                $ Forward

                when (kc == 9 && focusOverridesEsc xman) . loop
                                                         . unfocus
                                                         $ xman

            ExposeEvent _ _ _ _ _ _ _ _ _ _ -> do
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

                allocaXEvent $ \ev -> do
                    setEventType ev expose
                    sendEvent display xmenuw False exposureMask ev

                loop =<< runXMEvents xman eventQueue

            _   -> do

                loop xman

    let labelProps = (\lbl -> lbl { l_gen = (l_gen lbl)
                                            { gp_border = True } })

    let list = createListE "listExecs" 20 90 360 100 35
             . map (\(no, str) -> listLabelE ("listLabel" ++ show no) str 0 labelProps)
             . zip [1..]
             $ [ ("E1")
               , ("E2")
               , ("E3")
               , ("E4")
               , ("E5")
               , ("E6")
               , ("E7")
               ]

    let xman = runReader' xmglob $ createManager

                [ (emptyLabelE "inputLabel" 20 20 360 50 $ \lbl -> lbl
                               { l_gen = (l_gen lbl)
                                         { gp_border = True
                                         , gp_overridesEsc = True
                                         }
                               , l_cbs = (l_cbs lbl)
                                         { cb_onChange = Just $ \l -> do
                                        (flip runReaderT) xmdata $
                                            sendXMEvent eventQueue (\xm -> do
                                                let XMListE list' = getElement xm 1
                                                let listEls = sort . filter (isPrefixOf . l_val $ l) $ execList
                                                let newl' = map (\(no, str) -> listLabelE ("lblEl" ++ show no) str 0 labelProps xmglob) . zip [1..] $ listEls
                                                let newl = resetList . setElementsFromList list' $ newl'
                                                putStrLn $ "Got " ++ (show . length $ execList)
                                                return $ replaceElement xm 1 (XMListE newl))
                                        return l }
                               --, l_onReturn = \l -> return . (return l) <=< putStrLn . show . l_val $ l
                               })
                , (list $ \lst -> lst { li_gen = (li_gen lst)
                                                 { gp_overridesEsc = True
                                                 , gp_border = True }
                                      })
                ]

    loop $ xman { xem_inFocus = Nothing }

    freeFont display fontstr

    when (debug) $ putStrLn "Done."
