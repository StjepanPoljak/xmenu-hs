module XWindow (createXMenu) where

import Graphics.X11
import Graphics.X11.Xlib

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, ReaderT(..))
import XMenuGlobal

createXMenu :: ReaderT XMenuOpts IO XMenuGlobal
createXMenu = ask >>= \xmopts -> liftIO $ do

    display <- openDisplay ""
    let screen = defaultScreenOfDisplay display
    let scrnum = screenNumberOfScreen screen
    rootw <- rootWindow display scrnum

    let xmpos = getXmenuPosition screen (g_width xmopts) (g_height xmopts)

    window <- allocaSetWindowAttributes
        (\attributes -> do
            set_override_redirect attributes True
            createWindow display rootw (fst xmpos) (snd xmpos)
                           (g_width xmopts) (g_height xmopts) 1
                           (defaultDepthOfScreen screen)
                           inputOutput (defaultVisualOfScreen screen)
                           (cWOverrideRedirect) attributes
        )

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
