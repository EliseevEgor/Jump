{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PackageImports #-}

import Back
import Types
import Control.Concurrent (threadDelay)
import Control.Monad
  ( join,
    unless,
  )
import Control.Monad.Fix (fix)
import FRP.Elerea.Simple
import Game
import Graphics.Gloss.Rendering
import "GLFW-b" Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLUT as GLUT
import System.Exit (exitSuccess)

main :: IO ()
main = do
  (_, _) <- GLUT.getArgsAndInitialize
  (directionKeyGen, directionKeySink) <- external (False, False, False)
  (sKeyGen, sKeySink) <- external False
  (windowSizeGen, _) <-
    external
      (fromIntegral width, fromIntegral height)
  glossState <- initState
  withWindow width height "Jump" $ \win -> do
    network <- start $ do
      directionKey <- directionKeyGen
      windowSize <- windowSizeGen
      sKey <- sKeyGen
      jump win windowSize directionKey sKey glossState
    fix $ \loop -> do
      readInput win directionKeySink sKeySink
      join network
      threadDelay 25000
      esc <- keyIsPressed win Key'Escape
      unless esc loop
    exitSuccess