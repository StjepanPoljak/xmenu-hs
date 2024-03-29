module XManagerClass ( XEManager(..)
                     , createManager
                     , XEManagerClass(..)
                     , XEFocusDirection(..)
                     , direction
                     , XMItems
                     , XMAnimation(..)
                     , XMAnimationDuration(..)
                     , defaultEventMap
                     ) where

import Graphics.X11 (KeyCode, KeySym, xK_Escape, xK_Tab)

import Data.Bool (bool)
import Data.Maybe (isJust)
import qualified Data.Sequence as S ( Seq, fromList, mapWithIndex, null, (!?)
                                    , index, update, findIndexL, length
                                    )
import qualified Data.Foldable as F (toList)
import qualified Data.Map as M (Map, fromList, (!?))
import Data.Function ((&))
import Data.List (delete)

import Control.Concurrent ( forkOS, threadDelay, ThreadId
                          , myThreadId, killThread
                          )
import Control.Monad ((<=<), liftM, when, void)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Reader (Reader, ask, liftIO, mfilter)

import XElementClass as XE
import XContext
import XMenuGlobal
import XEvent

type XMItems a = S.Seq a
type XMMap a = M.Map String Int

data XEManager a = XEManager { xem_elements         :: XMItems a
                             , xem_inFocus          :: Maybe Int
                             , xem_map              :: Maybe (XMMap a)
                             , xem_eventMap         :: XMEventMap (XEManager a)
                             , xem_animations       :: [ThreadId]
                             }

data XEFocusDirection = Forward
                      | Backward
                      deriving (Eq)

direction :: a -> a -> XEFocusDirection -> a
direction x _ Forward = x
direction _ x Backward = x

data XMAnimationDuration = XMAnimateForever Int | XMAnimateFor Int

data XMAnimation a = XMAnimation { ani_fn       :: Int -> a -> a
                                 , ani_delay    :: Int
                                 , ani_duration :: XMAnimationDuration
                                 }

class XEManagerClass f where

    getAnimations :: (XMElementClass a) => f a -> [ThreadId]
    setAnimations :: (XMElementClass a) => f a -> [ThreadId] -> f a

    addAnimation :: (XMElementClass a) => f a -> ThreadId -> f a
    addAnimation xem anid = setAnimations xem
                          . (anid:)
                          . getAnimations $ xem

    delAnimation :: (XMElementClass a) => f a -> ThreadId -> f a
    delAnimation xem anid = setAnimations xem
                          . delete anid
                          . getAnimations $ xem

    cancelAnimation :: (XMElementClass a) => f a -> ThreadId -> IO (f a)
    cancelAnimation xem anid = killThread anid
                           >>= const (return $ delAnimation xem anid)

    animate :: (XMElementClass a) => XMEventQueue (f a) -> Int -> XMAnimation a
                                  -> XMenuDataM ThreadId
    animate evq el ani = ask >>= \xmdata -> liftIO $ forkOS $ do
        let animateLoop cnt = do

             tid <- liftIO $ myThreadId

             let cnt' = cnt + 1

             let modA xm = if cnt == 0
                           then addAnimation xm tid
                           else case ani_duration ani of
                             XMAnimateForever _  -> xm
                             XMAnimateFor times  -> if times == cnt'
                                                    then delAnimation xm tid
                                                    else xm

             (flip sendXMGUIEvent) evq $ \xman -> return
                                                . Just
                                                . modA
                                                . replaceElement xman el
                                                . (ani_fn ani) cnt
                                                . getElement xman
                                                $ el

             let loopWithDelay n = do
                  liftIO $ threadDelay (ani_delay ani)
                  animateLoop n

             case ani_duration ani of
                 XMAnimateForever m  -> loopWithDelay (cnt' `mod` m)
                 XMAnimateFor times  -> let
                                        in bool (return ())
                                                (loopWithDelay cnt')
                                         $ cnt' >= times

        runReaderT (animateLoop 0) xmdata

    getElements :: (XMElementClass a) => f a -> XMItems a
    getFocus :: (XMElementClass a) => f a -> Maybe Int
    setFocus :: (XMElementClass a) => f a -> Maybe Int -> f a
    setElements :: (XMElementClass a) => f a -> XMItems a -> f a

    getMap :: (XMElementClass a) => f a -> Maybe (XMMap a)
    setMap :: (XMElementClass a) => f a -> Maybe (XMMap a) -> f a

    getEventMap :: (XMElementClass a) => f a -> XMEventMap (f a)

    generateMap :: (XMElementClass a) => f a -> f a
    generateMap xem = setMap xem
                    . Just
                    . M.fromList
                    . F.toList
                    . S.mapWithIndex (\i el -> (flip (,)) i
                                             . gp_name
                                             . getGenProps $ el)
                    . getElements $ xem

    isEmpty :: (XMElementClass a) => f a -> Bool
    isEmpty = S.null . getElements

    showElements :: (XMElementClass a) => f a -> [(Int, String)]
    showElements = F.toList
                 . S.mapWithIndex (\i el -> (i, gp_name (getGenProps el)))
                 . getElements

    setElementsFromList :: (XMElementClass a) => f a -> [a] -> f a
    setElementsFromList xem list = setElements xem (S.fromList list)

    getElement :: (XMElementClass a) => f a -> Int -> a
    getElement xem no = getElements xem `S.index` no

    getElementNoByName :: (XMElementClass a) => f a -> String -> Maybe Int
    getElementNoByName xem name = case getMap xem of

            Nothing     -> S.findIndexL ((==name)
                                       . gp_name
                                       . getGenProps)
                         . getElements $ xem

            Just map    -> map M.!? name

    getElementByName :: (XMElementClass a) => f a -> String -> Maybe a
    getElementByName xem str = liftM (getElement xem)
                             $ getElementNoByName xem str

    modifyElementByName :: (XMElementClass a) => f a -> String
                                              -> (a -> a) -> f a
    modifyElementByName xem name f = maybe xem (\no -> replaceElement xem no
                                                     . f
                                                     $ getElement xem no)
                                   $ getElementNoByName xem name

    replaceElement :: (XMElementClass a) => f a -> Int -> a -> f a
    replaceElement xem no el = setElements xem
                             . S.update no el
                             . getElements $ xem

    forwardKey :: (XMElementClass a) => f a -> KeySym -> XMEventQueue (f a)
               -> Int -> XMenuDataM (f a)
    forwardKey xem ks evq el = do
                (nwel, rdrw) <- liftIO $ sendKeyInput (getElement xem el) ks
                when (rdrw) $ sendRedrawEvent evq
                return . replaceElement xem el $ nwel

    sendKeyInputToManager :: (XMElementClass a) => f a -> KeySym
                          -> XMEventQueue (f a) -> XMenuDataM ()
    sendKeyInputToManager xem ks evq =
                ask >>= \xmd -> maybe (sendXMEvent (keyEvent xmd) evq)
                                      (evq&)
                              . getXMEvent (getEventMap xem)
                              $ XMKeyEvent ks

          where forwardKey xem' foc = (\(nwel, rdrw) -> return
                                                      . (flip (,)) rdrw
                                                      . replaceElement xem' foc
                                                      $ nwel)
                                  <=< (flip sendKeyInput) ks
                                    . getElement xem' $ foc
                keyEvent xmd xem' = do
                        (nwxm, rdrw) <- liftIO $ maybe (return (xem', False))
                                                       (forwardKey xem')
                                               . getFocus $ xem'
                        when (rdrw) $ runReaderT (sendRedrawEvent evq) xmd
                        return (Just nwxm)

    drawAll :: (XMElementClass a) => f a -> XMContext
            -> ReaderT XMenuData IO ()
    drawAll xem ctx = mapM_ (\(i, e) -> drawElement ctx e (isJust
                                                         . mfilter (i==)
                                                         . getFocus $ xem))
                    . F.toList
                    . S.mapWithIndex (,)
                    . getElements $ xem

    focusOverridesEsc :: (XMElementClass a) => f a -> Bool
    focusOverridesEsc xem = maybe False
                                  (gp_overridesEsc . getGenProps
                                                   . getElement xem)
                                  (getFocus xem)

    changeFocus :: (XMElementClass a) => f a -> XEFocusDirection -> f a
    changeFocus xem dir
        | S.null . getElements $ xem = xem
        | otherwise  = let foc = getFocus xem
                       in changeFocus' foc xem dir
                        . direction (maybe 0
                                           (nextElement xem))
                                    (maybe (xemCount xem - 1)
                                           (prevElement xem))
                                    dir
                        $ foc

        where changeFocus' foc xem' dir curr
                | canFocus (getElement xem' curr) ||
                  maybe (curr + 1 == xemCount xem') (curr==) foc
                            = bool (setFocus xem' . Just $ curr)
                                   xem'
                            . isJust
                            . mfilter (curr==)
                            $ foc

                | otherwise = changeFocus' foc xem' dir
                            . nextElement xem'
                            $ curr

              nextElement xem' curr
                | curr + 1 == xemCount xem' = 0
                | otherwise                 = curr + 1

              prevElement xem' curr
                | curr == 0     = xemCount xem' - 1
                | otherwise     = curr - 1

              xemCount xem' = S.length . getElements $ xem'

instance XEManagerClass (XEManager) where
    getElements = xem_elements
    setElements xem els = xem { xem_elements = els }
    getFocus = xem_inFocus
    setFocus xem foc = xem { xem_inFocus = mfilter (isJust
                                                 . (S.!?) (xem_elements xem))
                                                   foc }
    getMap = xem_map
    setMap xem map = xem { xem_map = map }

    getEventMap = xem_eventMap

    getAnimations = xem_animations
    setAnimations xem anids = xem { xem_animations = anids }

defaultEventMap :: (XEManagerClass a, XMElementClass b) => XMEventMap (a b)
defaultEventMap = eventMapFromList
                              [ ( XMKeyEvent xK_Escape, sendXMGUIEvent
                                $ (\xman -> return
                                          . bool Nothing
                                                 (Just $ setFocus xman Nothing)
                                          $ focusOverridesEsc xman)
                                )
                              ,
                                ( XMKeyEvent xK_Tab, sendXMGUIEvent
                                        $ return
                                        . Just
                                        . (flip changeFocus) Forward
                                )
                              ]

createManager :: (XMElementClass b) => [XMenuGlobal -> b]
              -> Reader XMenuGlobal (XEManager b)
createManager xels = return
                   . (\els -> XEManager els Nothing Nothing defaultEventMap [])
                   . S.fromList
                   . (flip map) xels
                   . (&)
                 =<< ask

