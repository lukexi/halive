module HotGLFW where


import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent
import Graphics.GL
import Graphics.GL.Pal

import System.Random
import Data.Time
import Control.Monad

import HaliveUtils
import Cube
import Linear

import Data.Bits
import SetupGLFW

import qualified Green as Green -- Try changing the green amount 
                                -- while the program is running

main :: IO ()
main = do
    -- error "DANG!" -- It's ok if your program crashes, it shouldn't crash Halive
    win <- acquireGLFW
    GLFW.setWindowTitle win "Hot Swap!"

    program <- createShaderProgram "test/cube.vert" "test/cube.frag"
    cube <- makeCube program

    glEnable GL_DEPTH_TEST

    -- do -- swap this with the next line to test immediately-returning mains
    forever $ do
        GLFW.pollEvents
        now <- realToFrac . utctDayTime <$> getCurrentTime
        let redFreq  = 2 * pi -- Try changing the red and blue frequencies.
            red      = sin (now * redFreq)
            blueFreq = 1 * pi
            blue     = sin (now * blueFreq)
        glClearColor red Green.green blue 1
        glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
        -- Render our scene
        let projection = perspective 45 (640/480) 0.01 1000
            model      = mkTransformation 1 (V3 0 0 (-4))
            view       = lookAt (V3 0 2 5) (V3 0 0 (-4)) (V3 0 1 0)
            mvp        = projection !*! view !*! model
        
        renderCube cube mvp
        GLFW.swapBuffers win

-- GLFW only likes to be initialized once in a given process.
-- So we use foreign-store to only start it up once at the beginning,
-- and then store away a persistent reference that we can grab on
-- subsequent recompilations.
acquireGLFW :: IO GLFW.Window
acquireGLFW = reacquire 0 (setupGLFW "HotGLFW" 640 480)

