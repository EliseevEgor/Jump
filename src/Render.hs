{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PackageImports #-}

module Render where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Rendering
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Types

-- main function in graphics
renderFrame :: GLFW.Window -> State -> (Float, Float) -> RenderState -> IO ()
renderFrame window glossState _ (RenderState player monsters boxes floors coins flag gameOver viewport lives score timer mushrooms mbAnimation dimensions ) =
  do
    displayPicture dimensions white glossState (viewPortScale viewport) $
      Pictures $
        animation mbAnimation dimensions $
          gameOngoing gameOver $
            gameStats
              lives
              score
              timer
              dimensions
              [ renderPlayer player,
                uncurry translate (viewPortTranslate viewport) $
                  Pictures $
                    map
                      renderMonster
                      (filter isAlive monsters),
                uncurry translate (viewPortTranslate viewport) $
                  Pictures $
                    map
                      renderCoin
                      (filter vis coins),
                uncurry translate (viewPortTranslate viewport) (renderFlag flag),
                uncurry translate (viewPortTranslate viewport) $
                  Pictures $
                    map
                      renderBox
                      boxes,
                uncurry translate (viewPortTranslate viewport) $
                  Pictures $
                    map
                      renderFloor
                      floors,
                uncurry translate (viewPortTranslate viewport) $
                  Pictures $
                    map
                      renderMushroom
                      (filter v mushrooms)
              ]
    swapBuffers window
renderFrame window glossState _ (StartRenderState dimensions) = do
  displayPicture dimensions black glossState 1 $
    Pictures
      [ Color green $ translate (-140) 0 $ scale 0.4 0.4 $ Text "Jump",
        Color green $ translate (-140) (-50) $ scale 0.1 0.1 $ Text "Press s"
      ]
  swapBuffers window

renderMushroom :: Mushroom -> Picture
renderMushroom (Mushroom x y _) = 
  Color red $ translate x y $ circleSolid (mushroomSize / 2)

renderFlag :: Flag -> Picture
renderFlag (Flag xf yf _) =
  Color green $ translate xf yf $ rectangleSolid 10 300

renderMonster :: Monster -> Picture
renderMonster (Monster (x, y) _ _ _) =
  Color green $ translate x y $ circleSolid (monsterSize / 2)

renderCoin :: Coin -> Picture
renderCoin (Coin x y _) = Color yellow $ translate x y $ circleSolid coinSize

renderFloor :: Box -> Picture
renderFloor (Box x y w h) = Color orange $ translate x y $ rectangleSolid w h

renderBox :: Box -> Picture
renderBox (Box x y w h) = Color (greyN 0.5) $ translate x y $ rectangleSolid w h

renderPlayer :: Player -> Picture
renderPlayer (Player (_, ypos) _ pSize) =
  Color black $ translate 0 ypos $ rectangleSolid pSize pSize

-- finish level
gameOngoing :: Maybe Ending -> [Picture] -> [Picture]
gameOngoing (Just Lose) pics =
  pics ++ [Color black $ translate (-100) 0 $ Scale 0.3 0.3 $ Text "Game Over"]
gameOngoing (Just Win) pics =
  pics ++ [Color black $ translate (-100) 0 $ Scale 0.3 0.3 $ Text "Win, next level"]
gameOngoing Nothing pics = pics

-- store, lives, timer
gameStats :: Int -> Float -> Float -> (Int, Int) -> [Picture] -> [Picture]
gameStats lives score timer (w, h) pics = do
  let fWidth = fromIntegral w
      fHeight = fromIntegral h
  pics
    ++ [ Color black $
           translate (fWidth / 2 - 150) (fHeight / 2 - 50) $
             Scale 0.2 0.2 $
               Text $
                 "Score: " ++ show (round score),
         Color black $
           translate (-100) (fHeight / 2 - 50) $
             Scale 0.2 0.2 $
               Text $
                 "Timer: " ++ show (round timer),
         Color (makeColor 1 1 1 0.5) $
           translate ((- fWidth / 2) + 80) (fHeight / 2 - 40) $
             rectangleSolid 250 40,
         Color black $
           translate ((- fWidth) / 2 + 20) (fHeight / 2 - 50) $
             Scale 0.2 0.2 $
               Text $
                 "Lives: " ++ show lives
       ]


animation :: Maybe Animation -> (Int, Int) -> [Picture] -> [Picture]
animation Nothing _ pics = pics
animation (Just (DeathAnimation _)) _ pics = pics
animation (Just (NextLevelAnimation l n)) (w, h) pics =
  pics
    ++ [ Color (animationColor n) $
           rectangleSolid (fromIntegral w) (fromIntegral h),
         Color white $ translate (-100) 0 $ scale 0.3 0.3 $ Text $ show l
       ]
  where
    animationColor i
      | n > 25 = makeColor 0 0 0 (0.04 * (50 - i))
      | otherwise = makeColor 0 0 0 (0.04 * i)