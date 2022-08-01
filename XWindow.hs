module XWindow ( createXMenu
               , mainLoop
               , runReaderT'
               , runReader'
               , sendXMGUIEvent) where

import Graphics.X11
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (getEvent, setEventType, Event(..))

import Data.Bits ((.&.), (.|.))
import qualified Data.Map as M (Map, fromList, (!?))
import Data.Bool (bool)
import Data.Function ((&))

import Control.Monad (liftM, when, void, (<=<))
import Control.Monad.Reader (runReader, liftIO)
import Control.Monad.Trans.Reader (ask, ReaderT(..))

import XMenuGlobal
import XManagerClass
import XElementClass
import XEvent
import XContext

debug = True

createXMenu :: ReaderT XMenuOpts IO XMenuGlobal
createXMenu = ask >>= \xmopts -> liftIO $ do

    display <- openDisplay ""
    let screen = defaultScreenOfDisplay display
    let scrnum = screenNumberOfScreen screen
    rootw <- rootWindow display scrnum

    let (x, y) = getXmenuPosition screen (g_width xmopts) (g_height xmopts)

    window <- allocaSetWindowAttributes $
        \attributes -> do
            set_override_redirect attributes True
            createWindow display rootw x y (g_width xmopts) (g_height xmopts)
                         1 (defaultDepthOfScreen screen) inputOutput
                         (defaultVisualOfScreen screen) (cWOverrideRedirect)
                         attributes

    fontstr <- loadQueryFont display (g_font xmopts)

    return $ XMenuGlobal xmopts
                         (XMenuData display screen scrnum fontstr window)

    where getScreenCenter :: Screen -> (Position, Position)
          getScreenCenter scr = ((`div` 2) . fromIntegral
                                           . widthOfScreen $ scr,
                                 (`div` 2) . fromIntegral
                                           . heightOfScreen $ scr)
          getXmenuPosition :: Screen -> Dimension -> Dimension
                                     -> (Position, Position)
          getXmenuPosition s w h = let center = getScreenCenter s
                                   in (fst center - (fromIntegral w) `div` 2,
                                       snd center - (fromIntegral h) `div` 2)

initColor :: Display -> ScreenNumber -> String -> IO Pixel
initColor d s color = liftM (color_pixel . fst)
                    $ allocNamedColor d (defaultColormap d s) color

runReaderT' = flip runReaderT
runReader' = flip runReader

mainLoop :: (XEManagerClass a, XMElementClass b) => XMEventQueue a b
         -> XMenuGlobal -> (XMEventQueue a b -> XMenuDataM ()) -> a b -> IO ()
mainLoop evq xmg@(XMenuGlobal xmopts xmdata) onInit xman = do

    let display = g_display xmdata
    let xmenuw = g_xmenuw xmdata

    selectInput display xmenuw (exposureMask
                            .|. keyPressMask
                            .|. buttonPressMask
                            .|. focusChangeMask
                            .|. structureNotifyMask)

    mapWindow display xmenuw
    setInputFocus display xmenuw revertToPointerRoot 0

    mainLoop' evq xmg (False, onInit) xman

    freeFont display (g_fontStruct xmdata)

    when (debug) $ putStrLn "Done."

    return ()

type XKeySymMap a b = M.Map KeySym (XMEventQueue a b -> XMenuDataM ())

sendXMGUIEvent :: (XEManagerClass a, XMElementClass b)
               => XMEvent a b -> XMEventQueue a b
               -> XMenuDataM ()
sendXMGUIEvent cb tbq = do
                    sendXMEvent cb tbq
                    ask >>= \xmd -> sendXMEvent (\xm -> const (return $ Just xm) =<< runReaderT redraw xmd) tbq

defaultKeySymMap :: (XEManagerClass a, XMElementClass b) => XKeySymMap a b
defaultKeySymMap = M.fromList [ ( xK_Escape, sendXMGUIEvent
                                $ (\xman -> return
                                          . bool Nothing
                                                 (Just $ setFocus xman Nothing)
                                          $ focusOverridesEsc xman)
                                )
                              ,
                                ( xK_Tab, sendXMGUIEvent
                                        $ return
                                        . Just
                                        . (flip changeFocus) Forward
                                )
                              ]

redraw :: XMenuDataM ()
redraw = ask >>= \xmdata -> liftIO $ allocaXEvent
               $ \ev -> do setEventType ev expose
                           sendEvent (g_display xmdata) (g_xmenuw xmdata) False
                                     exposureMask ev

-- data XMenuEventData a b = XMenuEventData (Bool, XMenuEvent a b)

mainLoop' :: (XEManagerClass a, XMElementClass b) => XMEventQueue a b
         -> XMenuGlobal -> (Bool, XMEventQueue a b -> XMenuDataM ()) -> a b -> IO ()
mainLoop' evq xmg@(XMenuGlobal xmopts xmdata) app xman = do

    let display = g_display xmdata
    let xmenuw = g_xmenuw xmdata

    ev <- allocaXEvent $ \xptr -> do
        nextEvent display xptr
        event <- getEvent xptr
        return event

    case ev of

        ClientMessageEvent _ _ _ _ _ _ _ -> do

            maybe (return ()) (mainLoop' evq xmg app) =<< runXMEvents xman evq

        KeyEvent _ _ _ _ _ _ _ _ _ _ _ _ st kc _ -> do

            ksym <- keycodeToKeysym display kc
                  . fromIntegral $ st .&. (shiftMask .|. controlMask)

            mainLoop' evq xmg app =<< maybe (sendKeyInputToManager xman ksym)
                                            (const (return xman)
                                               <=< runReaderT' xmdata
                                                 . (evq &))
                                            (defaultKeySymMap M.!? ksym)

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

            when (not $ fst app) $ runReaderT ((snd app) evq) xmdata

            mainLoop' evq xmg (True, snd app) xman

--            runReaderT redraw xmdata

--            maybe (return ()) (mainLoop' evq xmg) =<< runXMEvents xman evq

        AnyEvent 10 _ _ _ _ -> do

            setInputFocus display xmenuw revertToPointerRoot 0
            mainLoop' evq xmg app xman

        _   -> do

            mainLoop' evq xmg app xman

