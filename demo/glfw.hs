module HotGLFW where

import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent
import Graphics.GL
import Linear
import System.Random
import Data.Time
import Control.Monad
import Data.Bits

import Halive.Utils
import Cube
import Shader
import SetupGLFW

import qualified Green as Green -- Try changing the green amount 
                                -- while the program is running

main :: IO ()
main = do
    -- error "DANG!" -- It's ok if your program crashes, it shouldn't crash Halive

    -- Wrap any persistent state in 'reacquire' plus a unique asset ID.
    -- Reacquire uses foreign-store to only run your setup function once,
    -- and then return it again on subsequent recompilations.
    -- In this case, GLFW doesn't like being initialized more than once
    -- per process, so this solves the problem handily. 
    -- (Our window stays persistent as well thanks to this, 
    -- so it would probably be a good idea anyway!)
    win <- reacquire 0 (setupGLFW "HotGLFW" 640 480)

    -- You can change the window title here.
    GLFW.setWindowTitle win "Hot Swap!"

    -- Changing the shaders' contents will trigger Halive as well!
    program <- createShaderProgram "test/cube.vert" "test/cube.frag"
    cube <- makeCube program

    -- Any GL state will stick around, so be aware of that.
    glEnable GL_DEPTH_TEST

    -- do -- swap this with the next line to test immediately-returning mains
    forever $ do
        GLFW.pollEvents
        now <- realToFrac . utctDayTime <$> getCurrentTime
        -- print now -- Try turning on a stream of now logs
        let redFreq  = 2 * pi -- Try changing the red and blue frequencies.
            red      = sin (now * redFreq)
            blueFreq = 1 * pi
            blue     = sin (now * blueFreq)
        glClearColor red Green.green blue 1
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        -- Render our scene
        (w,h) <- GLFW.getWindowSize win
        let projection = perspective 45 (fromIntegral w/ fromIntegral h) 0.01 1000
            model      = mkTransformation (axisAngle (V3 0 1 1) now) (V3 (sin now) 0 (-4))
            view       = lookAt (V3 0 2 5) (V3 0 0 (-4)) (V3 0 1 0)
            mvp        = projection !*! view !*! model
        
        renderCube cube mvp
        GLFW.swapBuffers win
