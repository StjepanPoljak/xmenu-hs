module Main where

import Graphics.X11.Xlib ( freeFont, exposureMask
                         , revertToParent
                         , mapWindow, setInputFocus, structureNotifyMask
                         , buttonPressMask, keyPressMask, selectInput)

import Data.Bits ((.|.))
import Data.Maybe (maybe)
import Data.List (isPrefixOf, concat, sort, nub)

import Control.Concurrent (forkIO)

import Control.Monad.Reader (runReader, liftIO)
import Control.Monad.Trans.Reader (runReaderT, ask, ReaderT)
import Control.Monad (when, (<=<), liftM, foldM, void)

import System.Environment (getEnv)
import System.Directory (getDirectoryContents)
import System.Process (createProcess, proc)

import XEvent
import XMenuGlobal
import XLabel
import XContext
import XManagerClass
import XElementClass
import XElement
import XList
import XWindow

debug = True

splitPathVar str = case dropWhile (== ':') str of
                        "" -> []
                        s' -> w : splitPathVar s''
                              where (w, s'') = break (==':') s'

getPathVar = liftM splitPathVar $ getEnv "PATH"

getFilesFromPathVar = foldM (\acc x ->
                                liftM (filter (not . (flip elem) [".", ".."])
                                     . concat . (:[acc]))
                                     $ getDirectoryContents x)
                            [] =<< getPathVar

main = do

    execList <- liftM nub getFilesFromPathVar

    eventQueue <- createEventQueue

    let xmopts = XMenuOpts 400 200 0x244758 0x12222a
                           (createFont "Terminus" 16)
                           10 5 15 15 0x6dcfff 0x12222a

    xmglob <- runReaderT createXMenu xmopts

    let (XMenuGlobal _ xmdata) = xmglob
    let (XMenuData display _ _ fontstr xmenuw) = xmdata

    selectInput display xmenuw (exposureMask
                            .|. keyPressMask
                            .|. buttonPressMask
                            .|. structureNotifyMask)

    mapWindow display xmenuw
    setInputFocus display xmenuw revertToParent 0

    let labelProps = (\lbl -> lbl { l_gen = (l_gen lbl)
                                            { gp_border = True } })

    let list = createListE "listExecs" 20 90 360 100 35
             . map (\(no, str) -> listLabelE ("listLabel" ++ show no)
                                             str 0 labelProps)
             . zip [1..]
             $ ([ ] :: [String])

    let inputLabelEvent str xm =
            return . Just
                   . replaceElement xm 1
                   . XMListE
                   . (flip setFocus) (Just 0)
                   . resetList
                   . setElementsFromList list'
                   . map (\(no, str) -> listLabelE ("lblEl" ++ show no)
                                        str 0 labelProps xmglob)
                   . zip [1..]
                   . sort
                   . filter (isPrefixOf str)
                   $ execList

            where XMListE list' = getElement xm 1

    let returnEvent str xm = do
            (const $ return Nothing) <=< forkIO $
                void $ createProcess (proc str [])

    let xman = runReader' xmglob $ createManager

                [ (emptyLabelE "inputLabel" 20 20 360 50 $ \lbl -> lbl
                    { l_gen = (l_gen lbl)
                              { gp_border = True
                              , gp_overridesEsc = True
                              }
                    , l_cbs = (l_cbs lbl)
                              { cb_onChange = Just $ \l -> do

                                        runReaderT' xmdata
                                            . sendXMEvent eventQueue
                                            . inputLabelEvent
                                            . l_val $ l
                                        return l

                              , cb_onKeyPress = Just $ \l (kc, str) ->

                                    (const $ return l) =<< case str of

                                        "Return"    -> void
                                                     . runReaderT' xmdata
                                                     . sendXMEvent eventQueue
                                                     . returnEvent
                                                     . l_val $ l

                                        _           -> return ()
                              }
                    })
                , (list $ \lst -> lst
                    { li_gen = (li_gen lst)
                               { gp_overridesEsc = True
                               , gp_border = True
                               }
                    })
                ]

    runReaderT' xmdata $ sendXMEvent eventQueue . return
                                                . inputLabelEvent ""
                                                $ xman

    mainLoop eventQueue xmglob $ xman { xem_inFocus = Nothing }

    freeFont display fontstr

    when (debug) $ putStrLn "Done."
