module Main where

import Graphics.X11.Xlib ( freeFont, exposureMask, revertToPointerRoot
                         , focusChangeMask , mapWindow, setInputFocus
                         , structureNotifyMask , buttonPressMask, keyPressMask
                         , selectInput, xK_Return)

import Data.Bits ((.|.))
import Data.List (isPrefixOf, concat, sort, nub)
import Data.Bool (bool)

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

getExecStr xm = case getFocus list of

            Nothing     -> l_val label
            Just foc    -> let focel = getElement list foc
                               XMLabelE lbllst = focel
                               inputVal = l_val label
                               listVal = l_val lbllst
                           in bool listVal (l_val label)
                                   (length (l_val label) > length listVal)
    where Just (XMLabelE label) = getElementByName xm "inputLabel"
          Just (XMListE list) = getElementByName xm "listExecs"

returnEvent xm = let strlst = words . getExecStr $ xm
                     comm = bool (head strlst) "" (null strlst)
                     args = bool (drop (length strlst - 1) strlst)
                                 []
                                 (length strlst <= 1)
                 in bool ((const $ return Nothing) <=< forkIO $
                            void $ createProcess (proc comm args))
                         (return $ Just xm)
                         (null comm)

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
                            .|. focusChangeMask
                            .|. structureNotifyMask)

    mapWindow display xmenuw
    setInputFocus display xmenuw revertToPointerRoot 0

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
                   . (flip resetList) (Just 0)
                   . setElementsFromList list
                   . map (\(no, str) -> listLabelE ("lblEl" ++ show no)
                                        str 0 labelProps xmglob)
                   . zip [1..]
                   . sort
                   . filter (isPrefixOf str)
                   $ execList

            where Just (XMListE list) = getElementByName xm "listExecs"

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

                              , cb_onKeyPress = Just $ \l ks ->

                                    bool (return ())
                                         (void . runReaderT' xmdata
                                               . sendXMEvent eventQueue
                                               $ returnEvent)
                                         (ks == xK_Return)
                                >>= (const $ return l)
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
