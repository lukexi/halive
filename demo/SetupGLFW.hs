{-# LANGUAGE LambdaCase #-}
module SetupGLFW where

import qualified Graphics.UI.GLFW as GLFW

import Control.Monad
import Data.Bool
import System.Exit
import System.IO

setupGLFW :: String -> Int -> Int -> IO GLFW.Window
setupGLFW windowName desiredW desiredH = do

    GLFW.setErrorCallback (Just (const (hPutStrLn stderr)))
    GLFW.init >>= bool (bail initFailMsg) (return ())

    GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL
    GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 1
    GLFW.windowHint $ GLFW.WindowHint'sRGBCapable True

    GLFW.createWindow desiredW desiredH windowName Nothing Nothing >>= \case
        Nothing -> bail cwFailMsg
        Just win -> do
            GLFW.makeContextCurrent (Just win)
            GLFW.swapInterval 1
            return win

  where
    initFailMsg = "Error: GLFW init() failed; perhaps $DISPLAY is not set."
    cwFailMsg = "Error: GLFW createWindow() failed; probably your GPU is too old."
    bail = hPutStrLn stderr >=> const exitFailure >=> undefined
