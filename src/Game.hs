{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecursiveDo #-}

module Game where

import Control.Applicative
  ( (<$>),
    (<*>),
    (<|>),
  )
import Control.Monad
  ( join,
    unless,
  )
import Data.List
import Data.Maybe
import FRP.Elerea.Simple
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Rendering
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Levels
import Render
import Types

-- main function
jump ::
  Window ->
  Signal (Int, Int) ->
  Signal (Bool, Bool, Bool) ->
  Signal Bool ->
  State ->
  SignalGen (Signal (IO ()))
jump win windowSize directionKey sKey glossState = mdo
  let mkGame = playGame windowSize directionKey sKey
  (gameState, gameTrigger) <- switch $ mkGame <$> gameStatus'
  gameStatus <- transfer Start gameProgress gameTrigger
  gameStatus' <- delay Start gameStatus
  return $ outputFunction win glossState <$> gameState
  where
    gameProgress False s = s
    gameProgress True Start = InGame
    gameProgress True InGame = Start

playGame ::
  Signal (Int, Int) ->
  Signal (Bool, Bool, Bool) ->
  Signal Bool ->
  GameStatus ->
  SignalGen (Signal GameState, Signal Bool)
playGame windowSize _ sKey Start = mdo
  let startGame = sIsPressed <$> sKey
      renderState = StartRenderState <$> windowSize
  return (GameState <$> renderState, startGame)
  where
    sIsPressed s = s

playGame windowSize directionKey sKey InGame = mdo
  (gameState, levelTrigger) <-
    switch $
      playLevel windowSize directionKey sKey
        <$> levelCount'
        <*> score'
        <*> lives'
        <*> timer'
  levelCount <- transfer2 initialLevel levelProgression gameState levelTrigger
  levelCount' <- delay initialLevel levelCount
  lives <- transfer2 initialLives decrementLives gameState levelTrigger
  lives' <- delay initialLives lives
  score <- memo (stateScore <$> gameState)
  score' <- delay (0 :: Float) score
  timer <- memo (stateTimer <$> gameState)
  timer' <- delay (300 :: Float) timer
  let gameOver = isGameOver <$> gameState
  return (gameState, gameOver)
  where
    isGameOver (GameState RenderState {renderState_lives = l}) = l == 0
    isGameOver (GameState (StartRenderState _)) = False
    stateScore (GameState RenderState {renderState_score = s}) = s
    stateScore (GameState (StartRenderState _)) = 0
    stateTimer (GameState RenderState {renderState_timer = _}) = 300
    stateTimer (GameState (StartRenderState _)) = 300
    decrementLives (GameState RenderState {renderState_ending = Just Lose}) True l =
      l - 1
    decrementLives (GameState _) _ l = l

levelProgression :: GameState -> Bool -> LevelStatus -> LevelStatus
levelProgression _ False level = level
levelProgression (GameState RenderState {renderState_ending = Just Win}) True (Level n) =
  Level (n + 1)
levelProgression (GameState RenderState {renderState_ending = Just Lose}) True level =
  level
levelProgression (GameState RenderState {renderState_ending = Nothing}) True level =
  level
levelProgression (GameState (StartRenderState _)) _ level = level

switch ::
  Signal (SignalGen (Signal GameState, Signal Bool)) ->
  SignalGen (Signal GameState, Signal Bool)
switch levelGen = mdo
  trigger <- memo (snd =<< gameSignal)
  trigger' <- delay True trigger
  maybeSignal <- generator (toMaybe <$> trigger' <*> levelGen)
  gameSignal <- transfer undefined store maybeSignal
  return (fst =<< gameSignal, trigger)
  where
    store (Just x) _ = x
    store Nothing x = x
    toMaybe bool x = if bool then Just <$> x else pure Nothing

-- function is responsible for one level
playLevel ::
  Signal (Int, Int) ->
  Signal (Bool, Bool, Bool) ->
  Signal Bool ->
  LevelStatus ->
  Float ->
  Int ->
  Float ->
  SignalGen (Signal GameState, Signal Bool)
playLevel windowSize directionKey _ level@(Level n) currentScore lives currentTimer =
  mdo
    let l =  getLevel (helpFunction n)
    player <- transfer4 initialPlayer (movePlayer 4 (levelBoxes l) (levelFloor l)) directionKey mushrooms' monsters' levelOver'
    flag <- transfer (levelFlag l) getFlag player

    floors <- stateful (levelFloor l) initM
    boxes <- stateful (levelBoxes l) initM
    coinsCount <- memo (countCoins <$> player <*> coins')
    coins <- transfer (levelCoins l) stateCoin player
    coins' <- delay (levelCoins l) coins

    mushrooms <- transfer (levelMushrooms l) stateMushroom player
    mushrooms' <- delay (levelMushrooms l) mushrooms

    timer <- stateful currentTimer decrement
    hits <- memo (countMonsters <$> player <*> monsters')
    monsters <-
      transfer2
        (levelMonsters l)
        stateMonster
        player
        levelOver'
    monsters' <- delay (levelMonsters l) monsters
    score <-
      transfer4
        currentScore
        accumulateScore
        hits
        coinsCount
        timer
        levelOver'
    levelOver <- memo (levelEnds <$> player <*> monsters <*> flag <*> timer)
    levelOver' <- delay Nothing levelOver
    animation <- transfer Nothing (endAnimation level) levelOver
    viewport <- transfer initialViewport viewPortMove player

    let renderState =
          RenderState
            <$> player
            <*> monsters
            <*> boxes
            <*> floors
            <*> coins
            <*> flag
            <*> levelOver
            <*> viewport
            <*> pure lives
            <*> score
            <*> timer
            <*> mushrooms 
            <*> animation
            <*> windowSize

    return (GameState <$> renderState, animationEnd <$> animation)
  where
    helper player@(Player (_, y) _ _ _) monsters timer
      | any (close player) monsters && not (bonus player)
          || y < floorLevel - floorHeight / 2
          || timer == 0 =
        Just Lose
      | otherwise = Nothing
    help player flag =
      if w (getFlag player flag) then Just Win else Nothing
    levelEnds player monsters flag timer =
      helper player monsters timer
        <|> help player flag

endAnimation :: LevelStatus -> Maybe Ending -> Maybe Animation -> Maybe Animation
endAnimation _ _ (Just (DeathAnimation 0)) = Just (DeathAnimation 0)
endAnimation _ _ (Just (NextLevelAnimation l 0)) =
  Just (NextLevelAnimation l 0)
endAnimation _ _ (Just (DeathAnimation n)) = Just (DeathAnimation (n - 1))
endAnimation _ _ (Just (NextLevelAnimation l n)) =
  Just (NextLevelAnimation l (n - 1))
endAnimation _ (Just Lose) _ = Just $ DeathAnimation 50
endAnimation (Level n) (Just Win) _ =
  Just $ NextLevelAnimation (Level (n + 1)) 50
endAnimation _ _ Nothing = Nothing

animationEnd :: Maybe Animation -> Bool
animationEnd (Just (DeathAnimation 0)) = True
animationEnd (Just (NextLevelAnimation _ 0)) = True
animationEnd _ = False

-- counter of taken coins
countCoins :: Player -> [Coin] -> Float
countCoins p coins =
  fromIntegral $
    length
      (filter (\x -> vis x && playerCoin p x == 1) coins)

bonusIs :: Player -> [Mushroom] -> Bool
bonusIs p = any (\x -> v x && playerMushroom p x == 1)   

-- counter of killed monsters   
countMonsters :: Player -> [Monster] -> Float
countMonsters p m =
  fromIntegral $
    length
      (filter (\x -> isAlive x && (playerMonster p x == 1)) m)

-- decrement for timer
decrement :: Float -> Float
decrement f = if f > 0 then f - 0.05 else 0

-- main function for coins
stateCoin :: Player -> [Coin] -> [Coin]
stateCoin p = fmap helper
  where
    helper (Coin x y v) =
      if v
        then case playerCoin p (Coin x y v) of
          1 -> Coin x y False
          0 -> Coin x y v
        else Coin x y v

stateMushroom :: Player -> [Mushroom] -> [Mushroom]
stateMushroom p = fmap helper
  where
    helper (Mushroom x y v) =
      if v
        then case playerMushroom p (Mushroom x y v) of
          1 -> Mushroom x y False
          0 -> Mushroom x y v
        else Mushroom x y v

-- main function for monsters
stateMonster :: Player -> Maybe Ending -> [Monster] -> [Monster]
stateMonster _ (Just _) monsters = monsters
stateMonster player _ monsters = fmap helper monsters
  where
    helper (Monster (x, y) d s is) =
      if is
        then case playerMonster player (Monster (x, y) d s is) of
          1 -> Monster (x, y) d s False
          2 -> Monster (x, y) d s True
          3 ->
            moveMonster
              (Monster (x, y) d s is)
        else Monster (x, y) d s is

-- main function for score
accumulateScore :: Float -> Float -> Float -> Maybe Ending -> Float -> Float
accumulateScore hits count timer (Just Win) score =
  score + hits + count + timer / 500
accumulateScore hits count _ _ score = score + hits + count

-- function for initial boxes
initM :: [Box] -> [Box]
initM b = b

dist :: (Float, Float) -> (Float, Float) -> Float
dist (x1, y1) (x2, y2) = (x2 - x1) ^ 2 + (y2 - y1) ^ 2

close :: Player -> Monster -> Bool
close (Player (x, y) _ pSize _) (Monster (xm, ym) _ _ is)
  | not is = False
  | (y - pSize / 2) < (ym + monsterSize / 2)
      && dist (x, y) (xm, ym) < (pSize / 2 + monsterSize / 2) ^ 2 =
    True
  | otherwise = False

-- player-coin interaction
playerCoin :: Player -> Coin -> Int
playerCoin (Player (x, y) _ pSize _) (Coin xc yc _) =
  if dist (x, y) (xc, yc) <= (pSize / 2 + coinSize) ^ 2 then 1 else 0

playerMushroom :: Player -> Mushroom -> Int
playerMushroom (Player (x, y) _ pSize _) (Mushroom xc yc _) =
  if dist (x, y) (xc, yc) <= (pSize / 2 + mushroomSize / 2) ^ 2 then 1 else 0

-- player-flag interaction
getFlag :: Player -> Flag -> Flag
getFlag (Player (x, _) _ _ _) (Flag xf yf w) =
  if x >= xf then Flag xf yf True else Flag xf yf w

-- player-monster interaction
playerMonster :: Player -> Monster -> Int
playerMonster (Player (x, y) _ pSize _) (Monster (xm, ym) _ _ _)
  | (y - pSize / 2) == (ym + monsterSize / 2)
      && (x + pSize / 2) > (xm - monsterSize / 2)
      && (x - pSize / 2) < (xm + monsterSize / 2) =
    1
  | (y - pSize / 2) < (ym + monsterSize / 2)
      && dist (x, y) (xm, ym) < (pSize / 2 + monsterSize / 2) ^ 2 =
    2
  | otherwise = 3

-- monster move function
moveMonster :: Monster -> Monster
moveMonster (Monster (x, y) d s False) = Monster (x, y) d s False
moveMonster (Monster (x, y) (GoLeft 0) step True) =
  Monster (x, y) (GoRight step) step True
moveMonster (Monster (x, y) (GoRight 0) step True) =
  Monster (x, y) (GoLeft step) step True
moveMonster (Monster (x, y) (GoLeft n) step True) =
  Monster (x - monsterSpeed, y) (GoLeft (n - 1)) step True
moveMonster (Monster (x, y) (GoRight n) step True) =
  Monster (x + monsterSpeed, y) (GoRight (n - 1)) step True

-- viewPort move function
viewPortMove :: Player -> ViewPort -> ViewPort
viewPortMove (Player (x, _) _ _ _) ViewPort {viewPortTranslate = _, viewPortRotate = rotation, viewPortScale = scaled} =
  ViewPort
    { viewPortTranslate = (- x, 0),
      viewPortRotate = rotation,
      viewPortScale = scaled
    }

takeBonus ::Player -> [Mushroom] -> Bool
takeBonus p = any (ddt p) where   
  ddt (Player (x, y) _ pSize _) (Mushroom xc yc _) =
    dist (x, y) (xc, yc) <= (pSize / 2 + mushroomSize / 2) ^ 2 + 5 

takeAwayBonus :: Player -> [Monster] -> Bool
takeAwayBonus p = any (close p)

movePlayer ::Float -> [Box] -> [Box] -> (Bool, Bool, Bool) -> [Mushroom] -> [Monster] -> Maybe Ending -> Player -> Player
movePlayer _ _ _ _ _ _ (Just _) (Player pos t personSize b) = Player pos t personSize b
movePlayer increment boxes floors direction mushrooms monsters Nothing player@(Player (x, y) t personSize b)
  | takeAwayBonus player monsters && b = move boxes floors direction (Player (x, y + 30) t (personSize - 10) False) increment
  | takeBonus player mushrooms && not b = move boxes floors direction (Player (x, y + 5) t (personSize + 10) True) increment
  | otherwise = move boxes floors direction (Player (x, y) t personSize b) increment

-- limits for player
limitDown :: [Box] -> [Box] -> Float -> Float -> Float -> Bool
limitDown boxes floors x y pSize =
  any
    ( \box ->
        (y - pSize / 2 == box_y box + box_height box / 2)
          && (x - pSize / 2 < box_x box + box_width box / 2)
          && (x + pSize / 2 > box_x box - box_width box / 2)
    )
    (boxes ++ floors)

limitUp :: [Box] -> Float -> Float -> Float -> Maybe Box
limitUp boxes x y pSize =
  find
    ( \box ->
        (y + pSize / 2 <= box_y box - boxSize / 2)
          && (y + pSize / 2 + 100 > box_y box - boxSize / 2)
          && (x - pSize / 2 < box_x box + boxSize / 2)
          && (x + pSize / 2 > box_x box - boxSize / 2)
    )
    boxes

limitLeft :: [Box] -> [Box] -> Float -> Float -> Float -> Float -> Maybe Box
limitLeft boxes floors x y pSize pSpeed =
  find
    ( \box ->
        (x - pSize / 2 >= box_x box + box_width box / 2)
          && (x - pSize / 2 + pSpeed < box_x box + box_width box / 2)
          && (y - pSize / 2 < box_y box + box_height box / 2)
          && (y + pSize / 2 > box_y box - box_height box / 2)
    )
    (boxes ++ floors)

limitRight :: [Box] -> [Box] -> Float -> Float -> Float -> Float -> Maybe Box
limitRight boxes floors x y pSize pSpeed =
  find
    ( \box ->
        (x + pSize / 2 <= box_x box - box_width box / 2)
          && (x + pSize / 2 + pSpeed > box_x box - box_width box / 2)
          && (y - pSize / 2 < box_y box + box_height box / 2)
          && (y + pSize / 2 > box_y box - box_height box / 2)
    )
    (boxes ++ floors)

-- acceleration
a :: Float
a = 1.2

-- pulse function
guardSpeedUp :: Float -> Float
guardSpeedUp t
  | t > 6 = t
  | otherwise = t + 0.5

guardSpeedDown :: Float -> Float
guardSpeedDown t
  | t <= 0 = 0
  | otherwise = t - 0.5


move :: [Box] -> [Box] -> (Bool, Bool, Bool) -> Player -> Float -> Player
move boxes floors (True, _, False) (Player (xpos, ypos) (tLeft, tRight) pSize b) increment =
  let boxL =
        limitLeft
          boxes
          floors
          xpos
          ypos
          pSize
          (- increment - a * tLeft + a * tRight)
      boxLg =
        limitLeft
          boxes
          floors
          xpos
          (ypos + gravity)
          pSize
          (- increment - a * tLeft + a * tRight)
   in ( if limitDown boxes floors xpos ypos pSize 
          then
            if isJust boxL
              then
                Player
                  ( box_x (fromJust boxL) + pSize / 2 + box_width (fromJust boxL) / 2,
                    ypos
                  )
                  (0, 0)
                  pSize
                  b
              else
                Player
                  (xpos - increment - a * tLeft + a * tRight, ypos)
                  (guardSpeedUp tLeft, guardSpeedDown tRight)
                  pSize
                  b
          else
            if isJust boxLg
              then
                Player
                  ( box_x (fromJust boxLg) + pSize / 2 + box_width (fromJust boxLg) / 2,
                    ypos + gravity
                  )
                  (0, 0)
                  pSize
                  b
              else
                Player
                  (xpos - increment - a * tLeft + a * tRight, ypos + gravity)
                  (tLeft, guardSpeedDown tRight)
                  pSize
                  b
      )
move boxes floors (_, True, False) (Player (xpos, ypos) (tLeft, tRight) pSize b) increment =
  let boxR =
        limitRight
          boxes
          floors
          xpos
          ypos
          pSize
          (increment + a * tRight - a * tLeft)
      boxRg =
        limitRight
          boxes
          floors
          xpos
          (ypos + gravity)
          pSize
          (increment + a * tRight - a * tLeft)
   in ( if limitDown boxes floors xpos ypos pSize
          then
            if isJust boxR
              then
                Player
                  ( box_x (fromJust boxR) - pSize / 2 - box_width (fromJust boxR) / 2,
                    ypos
                  )
                  (0, 0)
                  pSize
                  b
              else
                Player
                  (xpos + increment + a * tRight - a * tLeft, ypos)
                  (guardSpeedDown tLeft, guardSpeedUp tRight)
                  pSize
                  b
          else
            if isJust boxRg
              then
                Player
                  ( box_x (fromJust boxRg) - pSize / 2 - box_width (fromJust boxRg) / 2,
                    ypos + gravity
                  )
                  (0, 0)
                  pSize
                  b
              else
                Player
                  (xpos + increment + a * tRight - a * tLeft, ypos + gravity)
                  (guardSpeedDown tLeft, tRight)
                  pSize
                  b
      )
move boxes floors (True, _, True) (Player (xpos, ypos) (tLeft, tRight) pSize b) increment =
  let box = limitUp boxes xpos ypos pSize
      boxL =
        limitLeft
          boxes
          floors
          xpos
          ypos
          pSize
          (- increment - a * tLeft + a * tRight)
      boxLg =
        limitLeft
          boxes
          floors
          xpos
          (ypos + gravity)
          pSize
          (- increment - a * tLeft + a * tRight)
   in ( if limitDown boxes floors xpos ypos pSize
          then
            if isJust box
              then
                if isJust boxL
                  then
                    Player
                      ( box_x (fromJust boxL) + pSize / 2 + box_width (fromJust boxL) / 2,
                        box_y (fromJust box) - boxSize / 2 - pSize / 2
                      )
                      (0, 0)
                      pSize
                      b
                  else
                    Player
                      ( xpos - increment - a * tLeft + a * tRight,
                        box_y (fromJust box) - boxSize / 2 - pSize / 2
                      )
                      (guardSpeedUp tLeft, guardSpeedDown tRight)
                      pSize
                      b
              else
                if isJust boxL
                  then
                    Player
                      ( box_x (fromJust boxL)
                          + pSize
                          / 2
                          + box_width (fromJust boxL)
                          / 2,
                        ypos + 100
                      )
                      (0, 0)
                      pSize
                      b
                  else
                    Player
                      (xpos - increment - a * tLeft + a * tRight, ypos + 100)
                      (guardSpeedUp tLeft, guardSpeedDown tRight)
                      pSize
                      b
          else
            if isJust boxLg
              then
                Player
                  ( box_x (fromJust boxLg)
                      + pSize
                      / 2
                      + box_width (fromJust boxLg)
                      / 2,
                    ypos + gravity
                  )
                  (0, 0)
                  pSize
                  b
              else
                Player
                  (xpos - increment - a * tLeft + a * tRight, ypos + gravity)
                  (tLeft, guardSpeedDown tRight)
                  pSize
                  b
      )
move boxes floors (_, True, True) (Player (xpos, ypos) (tLeft, tRight) pSize b) increment =
  let box = limitUp boxes xpos ypos pSize
      boxR =
        limitRight
          boxes
          floors
          xpos
          ypos
          pSize
          (increment + a * tRight - a * tLeft)
      boxRg =
        limitRight
          boxes
          floors
          xpos
          (ypos + gravity)
          pSize
          (increment + a * tRight - a * tLeft)
   in ( if limitDown boxes floors xpos ypos pSize
          then
            if isJust box
              then
                if isJust boxR
                  then
                    Player
                      ( box_x (fromJust boxR)
                          - pSize
                          / 2
                          - box_width (fromJust boxR)
                          / 2,
                        box_y (fromJust box) - boxSize / 2 - pSize / 2
                      )
                      (0, 0)
                      pSize
                      b
                  else
                    Player
                      ( xpos + increment + a * tRight - a * tLeft,
                        box_y (fromJust box) - boxSize / 2 - pSize / 2
                      )
                      (guardSpeedDown tLeft, guardSpeedUp tRight)
                      pSize
                      b
              else
                if isJust boxR
                  then
                    Player
                      ( box_x (fromJust boxR)
                          - pSize
                          / 2
                          - box_width (fromJust boxR)
                          / 2,
                        ypos + 100
                      )
                      (0, 0)
                      pSize
                      b
                  else
                    Player
                      (xpos + increment + a * tRight - a * tLeft, ypos + 100)
                      (guardSpeedDown tLeft, guardSpeedUp tRight)
                      pSize
                      b
          else
            if isJust boxRg
              then
                Player
                  ( box_x (fromJust boxRg)
                      - pSize
                      / 2
                      - box_width (fromJust boxRg)
                      / 2,
                    ypos + gravity
                  )
                  (0, 0)
                  pSize
                  b
              else
                Player
                  (xpos + increment + a * tRight - a * tLeft, ypos + gravity)
                  (guardSpeedDown tLeft, tRight)
                  pSize
                  b
      )
move boxes floors (False, False, True) (Player (xpos, ypos) (tLeft, tRight) pSize b) increment =
  let box = limitUp boxes xpos ypos pSize
      boxR = limitRight boxes floors xpos ypos pSize (- a * tLeft + a * tRight)
      boxL = limitLeft boxes floors xpos ypos pSize (- a * tLeft + a * tRight)
      boxRg = limitRight boxes floors xpos (ypos + gravity) pSize (- a * tLeft + a * tRight)
      boxLg = limitLeft boxes floors xpos (ypos + gravity) pSize (- a * tLeft + a * tRight)
   in ( if limitDown boxes floors xpos ypos pSize
          then
            if isJust box
              then
                if isJust boxR
                  then
                    Player
                      ( box_x (fromJust boxR)
                          - pSize
                          / 2
                          - box_width (fromJust boxR)
                          / 2,
                        box_y (fromJust box) - boxSize / 2 - pSize / 2
                      )
                      (guardSpeedDown tLeft, guardSpeedDown tRight)
                      pSize
                      b
                  else
                    if isJust boxL
                      then
                        Player
                          ( box_x (fromJust boxL)
                              + pSize
                              / 2
                              + box_width (fromJust boxL)
                              / 2,
                            box_y (fromJust box) - boxSize / 2 - pSize / 2
                          )
                          (guardSpeedDown tLeft, guardSpeedDown tRight)
                          pSize
                          b
                      else
                        Player
                          ( xpos - a * tLeft + a * tRight,
                            box_y (fromJust box) - boxSize / 2 - pSize / 2
                          )
                          (guardSpeedDown tLeft, guardSpeedDown tRight)
                          pSize
                          b
              else
                if isJust boxR
                  then
                    Player
                      ( box_x (fromJust boxR)
                          - pSize
                          / 2
                          - box_width (fromJust boxR)
                          / 2,
                        ypos + 100
                      )
                      (guardSpeedDown tLeft, guardSpeedDown tRight)
                      pSize
                      b
                  else
                    if isJust boxL
                      then
                        Player
                          ( box_x (fromJust boxL)
                              + pSize
                              / 2
                              + box_width (fromJust boxL)
                              / 2,
                            ypos + 100
                          )
                          (guardSpeedDown tLeft, guardSpeedDown tRight)
                          pSize
                          b
                      else
                        Player
                          (xpos - a * tLeft + a * tRight, ypos + 100)
                          (guardSpeedDown tLeft, guardSpeedDown tRight)
                          pSize
                          b
          else
            if isJust boxRg
              then
                Player
                  ( box_x (fromJust boxRg) - pSize / 2 - box_width (fromJust boxRg) / 2,
                    ypos + gravity
                  )
                  (guardSpeedDown tLeft, guardSpeedDown tRight)
                  pSize
                  b
              else
                if isJust boxLg
                  then
                    Player
                      ( box_x (fromJust boxLg)
                          + pSize
                          / 2
                          + box_width (fromJust boxLg)
                          / 2,
                        ypos + gravity
                      )
                      (guardSpeedDown tLeft, guardSpeedDown tRight)
                      pSize
                      b
                  else
                    Player
                      (xpos - a * tLeft + a * tRight, ypos + gravity)
                      (guardSpeedDown tLeft, guardSpeedDown tRight)
                      pSize
                      b
      )
move boxes floors (False, False, False) (Player (xpos, ypos) (tLeft, tRight) pSize b) increment =
  let boxR =
        limitRight boxes floors xpos ypos pSize (- a * tLeft + a * tRight)
      boxL =
        limitLeft boxes floors xpos ypos pSize (- a * tLeft + a * tRight)
      boxRg =
        limitRight boxes floors xpos (ypos + gravity) pSize (- a * tLeft + a * tRight)
      boxLg =
        limitLeft boxes floors xpos (ypos + gravity) pSize (- a * tLeft + a * tRight)
   in ( if limitDown boxes floors xpos ypos pSize
          then
            if isJust boxR
              then
                Player
                  ( box_x (fromJust boxR)
                      - pSize
                      / 2
                      - box_width (fromJust boxR)
                      / 2,
                    ypos
                  )
                  (guardSpeedDown tLeft, guardSpeedDown tRight)
                  pSize
                  b
              else
                if isJust boxL
                  then
                    Player
                      ( box_x (fromJust boxL)
                          + pSize
                          / 2
                          + box_width (fromJust boxL)
                          / 2,
                        ypos
                      )
                      (guardSpeedDown tLeft, guardSpeedDown tRight)
                      pSize
                      b
                  else
                    Player
                      (xpos - a * tLeft + a * tRight, ypos)
                      (guardSpeedDown tLeft, guardSpeedDown tRight)
                      pSize
                      b
          else
            if isJust boxRg
              then
                Player
                  ( box_x (fromJust boxRg)
                      - pSize
                      / 2
                      - box_width (fromJust boxRg)
                      / 2,
                    ypos + gravity
                  )
                  (guardSpeedDown tLeft, guardSpeedDown tRight)
                  pSize
                  b
              else
                if isJust boxLg
                  then
                    Player
                      ( box_x (fromJust boxLg)
                          + pSize
                          / 2
                          + box_width (fromJust boxLg)
                          / 2,
                        ypos + gravity
                      )
                      (guardSpeedDown tLeft, guardSpeedDown tRight)
                      pSize
                      b
                  else
                    Player
                      (xpos - a * tLeft + a * tRight, ypos + gravity)
                      (guardSpeedDown tLeft, guardSpeedDown tRight)
                      pSize
                      b
      )

outputFunction :: GLFW.Window -> State -> GameState -> IO ()
outputFunction window glossState (GameState renderState) =
  renderFrame window glossState (worldWidth, worldHeight) renderState