module Main where

import Graphics.X11 (xK_Return, xK_Up, xK_Down, KeySym)

import Data.Bits ((.|.))
import Data.List (isPrefixOf, concat, sort, nub)
import Data.Bool (bool)
import Data.Function ((&))
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M ((!?), fromList)

import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad ((<=<), liftM, foldM, void, mfilter)

import System.Environment (getEnv)
import System.Directory (getDirectoryContents)
import System.Process (createProcess, proc)
import System.FilePath (getSearchPath)

import XEvent
import XMenuGlobal
import XLabel
import XContext
import XManagerClass
import XElementClass
import XElement
import XList
import XWindow

getFilesFromPathVar :: IO [String]
getFilesFromPathVar = foldM (\acc x ->
                                liftM (filter (not . (flip elem) [".", ".."])
                                     . concat . (:[acc]))
                                     $ getDirectoryContents x)
                            [] =<< getSearchPath

getExecStr :: (XEManagerClass f) => f XMElement -> String
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

returnEvent :: (XEManagerClass f) => XMEventCB (f XMElement)
returnEvent xm = let strlst = words . getExecStr $ xm
                     comm = bool (head strlst) "" (null strlst)
                     args = bool (drop (length strlst - 1) strlst)
                                 []
                                 (length strlst <= 1)
                 in bool ((const $ return Nothing) <=<
                            void $ createProcess (proc comm args))
                         (return $ Just xm)
                         (null comm)

arrowEvent :: (XEManagerClass f) => KeySym -> XMenuData
           -> XMEventQueue (f XMElement) -> XMEventCB (f XMElement)
arrowEvent ks xmdata evq xm = case getFocus xm of

    Just foc    -> ksToDir ks & case (gp_name . getGenProps
                                              . getElement xm) foc of

        "inputLabel"    -> direction (return . Just
                                             $ changeFocus xm Forward)
                                     (return $ Just xm)

        "listExecs"     -> direction (return . Just
                                           <=< runReaderT' xmdata
                                             $ forwardKey xm ks
                                                          evq foc
                                     )
                                     (bool (return . Just
                                                   $ changeFocus xm Backward)
                                           (return . Just
                                                 <=< runReaderT' xmdata
                                                   $ forwardKey xm ks
                                                                evq foc)
                                           (isJust . mfilter (>0)
                                                   . (\(XMListE l) ->
                                                            getFocus l)
                                                   $ getElement xm foc)
                                     )

    Nothing     -> return $ Just $ changeFocus xm Forward

    where ksToDir = fromJust
                  . (M.!?) (M.fromList [ (xK_Up, Backward)
                                       , (xK_Down, Forward)
                                       ])

labelProps :: XMLabel -> XMLabel
labelProps = (\lbl -> lbl { l_gen = (l_gen lbl)
                                    { gp_border = True } })

inputLabelEvent :: (XEManagerClass f) => XMenuGlobal -> [String] -> String
                -> XMEventCB (f XMElement)
inputLabelEvent xmglob execList str xm =

    return . Just
           . replaceElement xm 1
           . XMListE
           . (\lst -> lst { li_gen = (li_gen lst)
                                     { gp_canFocus = not
                                                   . null
                                                   . getElements
                                                   $ lst } } )
           . (flip resetList) (Just 0)
           . setElementsFromList list
           . map (\(no, str) -> listLabelE ("lblEl" ++ show no)
                                str 0 labelProps xmglob)
           . zip [1..]
           . sort
           . filter (isPrefixOf str)
           $ execList

    where Just (XMListE list) = getElementByName xm "listExecs"

main = do

    execList <- liftM nub getFilesFromPathVar

    evq <- createEventQueue

    let xmopts = XMenuOpts 400 200 0x244758 0x12222a
                           (createFont "Terminus" 16)
                           10 5 15 15 0x6dcfff 0x12222a

    xmglob <- runReaderT createXMenu xmopts

    let (XMenuGlobal _ xmdata) = xmglob
    let (XMenuData display _ _ fontstr xmenuw) = xmdata

    let xman = (runReader' xmglob $ createManager

                [ (emptyTextFieldE "inputLabel" 20 20 360 50 $ \lbl -> lbl
                    { l_gen = (l_gen lbl)
                              { gp_border = True
                              }
                    , l_events = eventMapFromList
                              [ (XMChangeEvent, (\l -> do

                                    runReaderT' xmdata
                                        . (flip sendXMGUIEvent) evq
                                        . inputLabelEvent xmglob execList
                                        . l_val $ l
                                    return l)
                                )
                              ]
                    })
                , ((createListE "listExecs" 20 90 360 100 35
                    . map (\(no, str) -> listLabelE ("listLabel" ++ show no)
                                                    str 0 labelProps)
                    . zip [1..] $ ([ ] :: [String])) $ \lst -> lst
                    { li_gen = (li_gen lst)
                               { gp_border = True
                               }
                    })
                ]) { xem_inFocus = Just 0
                   , xem_eventMap = (flip eventMapUnion) eventMap
                                  . eventMapFromList
                                  $ [ ( XMKeyEvent xK_Return
                                      , sendXMGUIEvent returnEvent
                                      )
                                    , ( XMKeyEvent xK_Up
                                      , sendXMGUIEvent $ arrowEvent xK_Up
                                                                    xmdata evq)
                                    , ( XMKeyEvent xK_Down
                                      , sendXMGUIEvent $ arrowEvent xK_Down
                                                                    xmdata evq)
                                    ]
                   }
            where eventMap = defaultEventMap

    let onInit = sendXMGUIEvent $ inputLabelEvent xmglob execList ""

    mainLoop evq xmglob onInit xman

