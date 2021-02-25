module Types where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort

initialPlayer :: Player
initialPlayer = Player (0, 0) (0, 0) playerSize

initialLevel :: LevelStatus
initialLevel = Level 1

initialLives :: Int
initialLives = 3

width :: Int
width = 800

height :: Int
height = 500

worldWidth :: Float
worldWidth = 15000

worldHeight :: Float
worldHeight = 500

gravity :: Float
gravity = -5

floorWidth :: Float
floorWidth = 150

floorHeight :: Float
floorHeight = 60

floorLevel :: Float
floorLevel = floorHeight - fromIntegral height / 2

boxSize :: Float
boxSize = 20

coinSize :: Float
coinSize = 10

playerSize :: Float
playerSize = 20

monsterSize :: Float
monsterSize = 20

monsterSpeed :: Float
monsterSpeed = 2.5


newtype GameState = GameState RenderState

data RenderState = RenderState { renderState_player :: Player
                               , renderState_monster :: [Monster]
                               , renderState_boxes :: [Box]
                               , renderState_floor :: [Box]
                               , renderState_coins :: [Coin]
                               , renderState_Flag :: Flag
                               , renderState_ending :: Maybe Ending
                               , renderState_viewport :: ViewPort
                               , renderState_lives :: Int
                               , renderState_score :: Float
                               , renderState_timer :: Float
                               , renderState_animation :: Maybe Animation
                               , renderState_windowSize :: (Int, Int) }
                 | StartRenderState (Int, Int)

type Pos = Point

data Animation = DeathAnimation Float | NextLevelAnimation LevelStatus Float
                 deriving Show

data LevelSettings = LevelSettings { levelPsize :: Float
                                   , levelMonsters :: [Monster]
                                   , levelCoins :: [Coin]
                                   , levelBoxes :: [Box]
                                   , levelFloor :: [Box]
                                   , levelFlag :: Flag
                                   }

data Coin = Coin
  { coin_x :: Float
  , coin_y :: Float
  , vis    :: Bool
  }

data Flag = Flag
  { flag_x :: Float
  , flag_y :: Float
  , w      :: Bool
  }

data Box = Box
  { box_x      :: Float
  , box_y      :: Float
  , box_width  :: Float
  , box_height :: Float
  }
  deriving Show

data Player = Player
  { position   :: Pos
  , timePlayer :: (Float, Float)
  , size       :: Float
  }

data Ending = Win | Lose
              deriving (Show, Eq)

data GameStatus = Start | InGame
                  deriving Show

newtype LevelStatus = Level Int
  deriving Show

data Monster = Monster
  { pos       :: Pos
  , direction :: Direction
  , monsterStep :: Int
  , isAlive   :: Bool
  }
data Direction = GoLeft {steps :: Int} | GoRight {steps :: Int}
