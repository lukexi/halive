{-# LANGUAGE ViewPatterns #-}
module Window where


import Control.Monad
import SDL
import Control.Monad.Trans
import Control.Exception
import Linear
import Linear.Affine
import Data.Text (Text)
import Control.Lens

createGLWindow :: MonadIO m => Text -> m (Window, GLContext)
createGLWindow windowName = do
    initialize
        [ InitVideo
        , InitEvents
        -- , InitJoystick , InitGameController
        ]
    window <- createWindow windowName defaultWindow
        { windowGraphicsContext = OpenGLContext $ defaultOpenGL
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


windowPosToWorldPos :: (Epsilon a, Real a, Floating a)
                    => V2 a
                    -> M44 a
                    -> V2 a
                    -> a
                    -> V3 a
windowPosToWorldPos winSize viewProj coord depth = rayStart + rayDir * realToFrac depth
    where
        V2 xNDC yNDC = win2Ndc coord winSize
        rayStart = ndc2Wld (V4 xNDC yNDC (-1.0) 1.0)
        rayEnd   = ndc2Wld (V4 xNDC yNDC 0.0    1.0)
        rayDir   = normalize (rayEnd ^-^ rayStart)
        -- Converts from window coordinates (origin top-left) to normalized device coordinates
        win2Ndc (V2 x y) (V2 w h) =
            V2 ((x / w - 0.5) * 2)
               ((((h - y) / h) - 0.5) * 2)
        -- Converts from normalized device coordinates to world coordinates
        ndc2Wld i = hom2Euc (invViewProj !* i)
        -- Converts from homogeneous coordinates to Euclidean coordinates
        hom2Euc v = (v ^/ (v ^. _w)) ^. _xyz
        invViewProj = inv44 viewProj


