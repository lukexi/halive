module HotGLFW where
import Foreign.Store

import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent
import Graphics.GL

import System.Random
import Data.Time
import Control.Monad

import qualified Green as Green -- Try changing the green amount 
                                -- while the program is running

main :: IO ()
main = do
    -- error "DANG!" -- It's ok if your program crashes, it shouldn't crash Halive
    win <- acquireGLFW
    GLFW.setWindowTitle win "Hot Swap!"
    -- do -- swap this with the next line to test immediately-returning mains
    forever $ do
        GLFW.pollEvents
        now <- realToFrac . utctDayTime <$> getCurrentTime
        let redFreq  = 2 * pi -- Try changing the red and blue frequencies.
            red      = sin (now * redFreq)
            blueFreq = 1 * pi
            blue     = sin (now * blueFreq)
        glClearColor red Green.green blue 1
        glClear GL_COLOR_BUFFER_BIT
        GLFW.swapBuffers win

-- GLFW only likes to be initialized once in a given process.
-- So we use foreign-store to only start it up once at the beginning,
-- and then store away a persistent reference that we can grab on
-- subsequent recompilations.
acquireGLFW :: IO GLFW.Window
acquireGLFW = do
    let storeID = 0
    putStrLn $ "Looking up store: " ++ show storeID
    maybeStore <- lookupStore storeID :: IO (Maybe (Store GLFW.Window))
    case maybeStore of
        Just store -> readStore store
        Nothing -> do
            putStrLn "Creating GLFW..."
            win <- setupGLFW "HotGLFW" 640 480
            writeStore (Store storeID) win
            return win


setupGLFW :: String -> Int -> Int -> IO GLFW.Window
setupGLFW windowName desiredW desiredH = do
    _ <- GLFW.init

    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 1
    GLFW.windowHint $ GLFW.WindowHint'sRGBCapable True

    Just win <- GLFW.createWindow desiredW desiredH windowName Nothing Nothing
    
    GLFW.makeContextCurrent (Just win)

    GLFW.swapInterval 1
    return win