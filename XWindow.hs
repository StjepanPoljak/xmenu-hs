module XWindow (createXMenu, mainLoop, runReaderT', runReader') where

import Graphics.X11
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (getEvent, setEventType, Event(..))

import Data.Bits ((.&.), (.|.))
import Control.Monad (liftM, unless, when)
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
         -> XMenuGlobal -> a b -> IO ()
mainLoop evq xmg@(XMenuGlobal xmopts xmdata) xman = do

    let display = g_display xmdata
    let xmenuw = g_xmenuw xmdata

    ev <- allocaXEvent $ \xptr -> do
        nextEvent display xptr
        event <- getEvent xptr
        return event

    case ev of

        ClientMessageEvent _ _ _ _ _ _ _ -> do

            mainLoop evq xmg xman

        KeyEvent _ _ _ _ _ _ _ _ _ _ _ _ st kc _ -> do

            ksym <- keycodeToKeysym display kc
                  . fromIntegral $ st .&. (shiftMask .|. controlMask)

            unless (ksym == xK_Escape) $ do

                -- let keyStr = keysymToString ksym

                when (debug) . putStrLn $ show (kc, keysymToString ksym, st)

                mainLoop evq xmg =<< sendKeyInputToManager xman ksym

            when (ksym == xK_Escape
              && focusOverridesEsc xman)
               . mainLoop evq xmg
               . (flip setFocus) Nothing
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

            maybe (return ()) (mainLoop evq xmg) =<< runXMEvents xman evq

        AnyEvent 10 _ _ _ _ -> do

            setInputFocus display xmenuw revertToPointerRoot 0
            mainLoop evq xmg xman

        _   -> do

            mainLoop evq xmg xman

