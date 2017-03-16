{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Graphics.GL
import Linear
import System.Random
import Data.Time
import Control.Monad
import Control.Monad.State
import Data.Bits
import Text.Show.Pretty
import Data.Maybe

import Halive.Utils
import Cube
import Shader
import Window

import SDL hiding (get)
import qualified SDL as SDL


import qualified Green as Green -- Try changing the green amount
                                -- while the program is running

main :: IO ()
main = do
    putStrLn "MAIN BEGIN"
    -- error "DANG!" -- It's ok if your program crashes, it shouldn't crash Halive

    -- Wrap any persistent state in 'reacquire' plus a unique asset ID.
    -- Reacquire uses foreign-store to only run your setup function once,
    -- and then return it again on subsequent recompilations.
    -- In this case, GLFW doesn't like being initialized more than once
    -- per process, so this solves the problem handily.
    -- (Our window stays persistent as well thanks to this,
    -- so it would probably be a good idea anyway!)

    (win, _ctx) <- reacquire 0 $ createGLWindow "Hot SDL"

    -- You can change the window title here.
    --GLFW.setWindowTitle win "Hot Swap!"

    -- Changing the shaders' contents will trigger Halive as well!
    program <- createShaderProgram "demo/cube.vert" "demo/cube.frag"
    cube <- makeCube program

    -- Any GL state will stick around, so be aware of that.
    glEnable GL_DEPTH_TEST

    -- Sometimes it's useful to know if we're running under Halive or not
    putStrLn . ("Running under Halive: " ++ ) . show =<< isHaliveActive

    -- Reacquire our state from the last run, if any - otherwise create a new state
    initialState <- reacquire 1 (return ([]::[V2 GLfloat]))
    void . flip runStateT initialState . whileWindow win $ \events -> do
        -- Store our state persistently in slot 1
        persistState 1

        winSize@(V2 w h) <- fmap realToFrac <$> SDL.get (SDL.windowSize win)

        forM_ (catMaybes $ map matchMouse events) $ \cursorPos -> do
            isMouseDown <- SDL.getMouseButtons
            when (isMouseDown ButtonLeft) $ do
                let V2 x y = (cursorPos / winSize) * 2 - 1
                    cursorPosNDC = V2 x (negate y)
                modify' ((cursorPosNDC :) . take 40)

        -- Try turning on a stream of events
        -- unless (null events) $
        --     liftIO $ pPrint events

        now <- realToFrac . utctDayTime <$> liftIO getCurrentTime
        -- print now -- Try turning on a stream of now logs
        let redFreq  = 0.6 * pi -- Try changing the red and blue frequencies.
            red      = sin (now * redFreq)
            blueFreq = 0.5 * pi
            blue     = sin (now * blueFreq)
        --putStrLn "glClearColor"
        glClearColor red 0.3 blue 1
        --putStrLn "glClear"
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        -- Render our scene
        --putStrLn "getWinSize"

        let projection = perspective 45 (w / h) 0.01 1000
            model      = mkTransformation (axisAngle (V3 0 1 1) now) (V3 (sin now) 0 (-4))
            view       = lookAt (V3 0 2 5) (V3 0 0 (-4)) (V3 0 1 0)
            projView   = projection !*! view
            mvp        = projView !*! model

        --putStrLn "renderCube"
        renderCube cube mvp

        positions <- get
        forM_ positions $ \cursorPos -> do
            let V2 x y = cursorPos * 20
                model = mkTransformation (axisAngle (V3 0 1 1) now) (V3 x y (-20))
                mvp   = projView !*! model
            renderCube cube mvp
        --putStrLn "glSwapWindow"
        SDL.glSwapWindow win


matchMouse Event
    { eventPayload =
        MouseMotionEvent
          MouseMotionEventData
            { mouseMotionEventWhich = Mouse 0
            , mouseMotionEventPos = P pos
            }
    }
    = Just (fromIntegral <$> pos)
matchMouse _
    = Nothing
