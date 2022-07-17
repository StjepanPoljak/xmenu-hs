module XEvent ( XMEvent
              , createEventQueue
              , sendXMEvent
              , runXMEvents
              , XMEventQueue
              ) where

import Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, writeTBQueue
                                      , tryReadTBQueue)
import Control.Concurrent.STM (atomically)
import XElementClass
import XManagerClass
import XMenuGlobal
import Control.Monad ((<=<))
import Control.Monad.Reader (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)

import Data.Function ((&))
import Graphics.X11.Xlib.Extras (setClientMessageEvent', setEventType)
import Graphics.X11.Xlib (sync, structureNotifyMask, sendEvent
                         , clientMessage, internAtom, allocaXEvent)

type XMEvent a b = a b -> IO (Maybe (a b))
type XMEventQueue a b = TBQueue (XMEvent a b)

createEventQueue :: (XEManagerClass a, XMElementClass b)
                 => IO (TBQueue (XMEvent a b))
createEventQueue = newTBQueueIO 128

sendXMEvent :: (XEManagerClass a, XMElementClass b)
            => TBQueue (XMEvent a b) -> XMEvent a b
            -> ReaderT XMenuData IO ()
sendXMEvent tbq cb = ask >>= \xmdata -> liftIO $ do
    atomically . writeTBQueue tbq $ cb
    xmonad_test <- internAtom (g_display xmdata) "XMONAD_TEST" False
    allocaXEvent $ \ev -> do
        setEventType ev clientMessage
        setClientMessageEvent' ev (g_xmenuw xmdata) xmonad_test 32 []
        sendEvent (g_display xmdata) (g_xmenuw xmdata) False
                  (structureNotifyMask) ev
    (flip sync) False . g_display $ xmdata

runXMEvents :: (XEManagerClass a, XMElementClass b) => a b
            -> TBQueue (XMEvent a b) -> IO (Maybe (a b))
runXMEvents xman tbq = maybe (return $ Just xman)
                             (maybe (return Nothing)
                                    ((flip runXMEvents) tbq)
                                <=< (xman &))
                   <=< atomically
                     . tryReadTBQueue $ tbq

