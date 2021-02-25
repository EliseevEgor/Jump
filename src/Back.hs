{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PackageImports #-}

module Back where

import Control.Monad
  ( unless,
    when,
  )
import "GLFW-b" Graphics.UI.GLFW as GLFW


withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      (Just win) -> do
        GLFW.makeContextCurrent m
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
      Nothing -> return ()
    GLFW.terminate
  where
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

readInput :: Window -> ((Bool, Bool, Bool) -> IO ()) -> (Bool -> IO ()) -> IO ()
readInput window directionKeySink skeySink = do
  pollEvents
  l <- keyIsPressed window Key'Left
  r <- keyIsPressed window Key'Right
  u <- keyIsPressed window Key'Space
  unless r (GLFW.setTime 0)
  directionKeySink (l, r, u)
  skeySink =<< keyIsPressed window Key'S

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed = True
isPress KeyState'Repeating = True
isPress _ = False