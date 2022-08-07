module XEvent ( XMEventCB
              , XMEvent(..)
              , XMEventMap
              , XMElEventMap
              , emptyEventMap
              , eventMapFromList
              , alterEventMap
              , eventMapUnion
              , removeEvent
              , getXMEvent
              , createEventQueue
              , sendXMEvent
              , sendRedrawEvent
              , sendXMGUIEvent
              , runXMEvents
              , XMEventQueue
              ) where

import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, writeTBQueue
                                      , tryReadTBQueue)
import Control.Concurrent.STM (atomically)
import XMenuGlobal
import Control.Monad ((<=<))
import Control.Monad.Reader (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import Data.Function ((&))
import qualified Data.Map as M (Map, empty, fromList, (!?)
                               , alter, union, delete)
import Graphics.X11.Xlib.Extras (setClientMessageEvent', setEventType)
import Graphics.X11.Xlib (sync, structureNotifyMask, sendEvent, KeySym
                         , clientMessage, internAtom, allocaXEvent
                         )

type XMEventCB a = a -> IO (Maybe a)
type XMEventQueue a = TBQueue (XMEventCB a)

data XMEvent = XMKeyEvent KeySym
             | XMChangeEvent
             deriving (Eq, Ord)

type XMEventMap a = M.Map XMEvent (XMEventQueue a -> XMenuDataM ())
type XMElEventMap a = M.Map XMEvent (a -> IO a)

emptyEventMap :: (Ord k) => M.Map k a
emptyEventMap = M.empty

eventMapFromList :: (Ord k) => [(k, a)] -> M.Map k a
eventMapFromList = M.fromList

getXMEvent :: (Ord k) => M.Map k a -> k -> Maybe a
getXMEvent = (M.!?)

alterEventMap :: (Ord k) => (Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a
alterEventMap = M.alter

eventMapUnion :: (Ord k) => M.Map k a -> M.Map k a -> M.Map k a
eventMapUnion = M.union

removeEvent :: (Ord k) => M.Map k a -> k -> M.Map k a
removeEvent m k = maybe m (const $ M.delete k m) . (M.!?) m $ k

createEventQueue :: IO (XMEventQueue a)
createEventQueue = newTBQueueIO 128

sendXMEvent :: XMEventCB a -> XMEventQueue a -> XMenuDataM ()
sendXMEvent cb tbq = ask >>= \xmdata -> liftIO $ do
    atomically . writeTBQueue tbq $ cb
    xmonad_test <- internAtom (g_display xmdata) "XMONAD_TEST" False
    allocaXEvent $ \ev -> do
        setEventType ev clientMessage
        setClientMessageEvent' ev (g_xmenuw xmdata) xmonad_test 32 []
        sendEvent (g_display xmdata) (g_xmenuw xmdata) False
                  (structureNotifyMask) ev
    (flip sync) False . g_display $ xmdata

sendRedrawEvent :: XMEventQueue a -> XMenuDataM ()
sendRedrawEvent tbq = ask >>= (flip sendXMEvent) tbq
                            . (flip (\xm -> const (return $ Just xm)
                                        <=< runReaderT redraw))

sendXMGUIEvent :: XMEventCB a -> XMEventQueue a -> XMenuDataM ()
sendXMGUIEvent cb tbq = sendXMEvent cb tbq >>= const (sendRedrawEvent tbq)

runXMEvents :: a -> XMEventQueue a -> IO (Maybe a)
runXMEvents xman tbq = maybe (return $ Just xman)
                             (maybe (return Nothing)
                                    ((flip runXMEvents) tbq)
                                <=< (xman &))
                   <=< atomically
                     . tryReadTBQueue $ tbq

