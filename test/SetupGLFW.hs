module SetupGLFW where

import qualified Graphics.UI.GLFW as GLFW

import Control.Monad

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