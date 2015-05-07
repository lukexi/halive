module HotGLFW where
import Foreign.Store

import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent
import Graphics.GL

import System.Random
import Green

main = do
    win <- acquireGLFW
    GLFW.setWindowTitle win "Hot Swap!"
    GLFW.pollEvents
    g <- (/1000) <$> randomRIO (0,500)
    b <- (/1000) <$> randomRIO (0,500)
    glClearColor 1 g b 1
    glClear GL_COLOR_BUFFER_BIT
    GLFW.swapBuffers win

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
