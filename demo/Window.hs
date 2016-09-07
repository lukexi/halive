module Window where


import Control.Monad
import SDL
import Control.Monad.Trans
import Control.Exception
import Linear
import Linear.Affine
import Data.Text (Text)

createGLWindow :: MonadIO m => Text -> m (Window, GLContext)
createGLWindow windowName = do
    initialize
        [ InitVideo
        , InitEvents
        -- , InitJoystick , InitGameController
        ]
    window <- createWindow windowName defaultWindow
        { windowOpenGL = Just $ defaultOpenGL
            { glProfile = Core Normal 4 1
            }
        , windowHighDPI = True
        , windowInitialSize = V2 640 480
        --, windowPosition = Centered
        , windowPosition = Absolute (P (V2 100 100))
        , windowResizable = True
        }
    glContext <- glCreateContext window
    glMakeCurrent window glContext
    swapInterval $= ImmediateUpdates
    return (window, glContext)

withWindow windowName = bracket (createGLWindow windowName)
    (\(win, ctx) -> do
        destroyWindow win
        glDeleteContext ctx)

whileWindow :: MonadIO m => Window -> ([Event] -> m a) -> m ()
whileWindow window action = do
    let loop = do
            --liftIO (putStrLn "pollEvents")
            events <- pollEvents
            --liftIO (putStrLn "action")
            _ <- action events
            let shouldQuit = (QuitEvent `elem`) $ map eventPayload events
            unless shouldQuit loop
    loop
    destroyWindow window
