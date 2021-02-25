-- file describing levels
{-# LANGUAGE ExtendedDefaultRules #-}

module Levels where

import Graphics.Gloss.Data.ViewPort
import Types

getLevel :: Int -> LevelSettings
getLevel n
  | n == 1 = level1
  | n == 2 = level2
  | n == 3 = level3
  | otherwise = level1

initialViewport :: ViewPort
initialViewport =
  ViewPort
    { viewPortTranslate = (0, 0),
      viewPortRotate = 0,
      viewPortScale = 4
    }

initialCoin :: (Float, Float) -> Coin
initialCoin (x, y) = Coin x y True

h :: Float
h = - fromIntegral height / 2

initialMonster :: Pos -> Int -> Monster
initialMonster pos monsterStep = Monster pos (GoLeft monsterStep) monsterStep True

initialBox :: (Float, Float) -> Box
initialBox (x, y) = Box x y boxSize boxSize

initialFloor :: (Float, Float) -> Box
initialFloor (x, y) = Box x y floorWidth floorHeight

level1 :: LevelSettings
level1 = LevelSettings 30 monstersLevel1 coinsLevel1 boxesLevel1 floorLevel1 flagLevel1

flagLevel1 :: Flag
flagLevel1 = Flag 4400 (70 + h) False

monstersLevel1 :: [Monster]
monstersLevel1 =
  [ initialMonster (450, 70 + h) 350,
    initialMonster (800, 70 + h) 35,
    initialMonster (1000, 70 + h) 55,
    Monster (855, 70 + h) (GoRight 55) 55 True,
    initialMonster (1500, 70 + h) 50,
    initialMonster (2100, 70 + h) 100,
    initialMonster (2200, 70 + h) 90,
    initialMonster (3900, 70 + h) 135,
    Monster (3560, 70 + h) (GoRight 135) 135 True
  ]

boxesLevel1 :: [Box]
boxesLevel1 =
  [initialBox (200, 120 + h)]
    ++ take 5 [initialBox (250 + x * 20, 120 + h) | x <- [1 ..]]
    ++ [initialBox (450 + x * 20, 50 + y * 20 + h) | x <- [1, 2], y <- [1, 2]]
    ++ [initialBox (650 + x * 20, 50 + y * 20 + h) | x <- [1, 2], y <- [1, 2, 3]]
    ++ [initialBox (800 + x * 20, 50 + y * 20 + h) | x <- [1, 2], y <- [1, 2, 3]]
    ++ [initialBox (1000 + x * 20, 50 + y * 20 + h) | x <- [1, 2], y <- [1, 2, 3]]
    ++ take 4 [initialBox (1450 + x * 20, 120 + h) | x <- [1 ..]]
    ++ take 7 [initialBox (1550 + x * 20, 190 + h) | x <- [1 ..]]
    ++ [initialBox (1870, 120 + h)]
    ++ take 4 [initialBox (1790 + x * 20, 190 + h) | x <- [1 ..]]
    ++ [ initialBox (2780, 130 + h),
         initialBox (2870, 130 + h),
         initialBox (3115, 130 + h),
         initialBox (3285, 130 + h),
         initialBox (4130, 230 + h)
       ]
    ++ [initialBox (1970 + x * 20, 120 + h) | x <- [1, 2]]
    ++ [initialBox (2100 + x * 20, 120 + h) | x <- [1, 3, 5]]
    ++ [initialBox (2160, 170 + h)]
    ++ [initialBox (2300, 120 + h)]
    ++ [initialBox (2400 + x * 20, 190 + h) | x <- [1, 2, 3]]
    ++ [initialBox (2570 + x * 20, 120 + h) | x <- [1, 2]]
    ++ [initialBox (2550 + x * 20, 190 + h) | x <- [1, 2, 3, 4]]
    ++ [initialBox (2700 + x * 20, 70 + h) | x <- [1, 2, 3, 4]]
    ++ [initialBox (2720 + x * 20, 90 + h) | x <- [1, 2, 3]]
    ++ [initialBox (2740 + x * 20, 110 + h) | x <- [1, 2]]
    ++ [initialBox (2850 + x * 20, 70 + h) | x <- [1, 2, 3, 4]]
    ++ [initialBox (2850 + x * 20, 70 + h) | x <- [1, 2, 3, 4]]
    ++ [initialBox (2850 + x * 20, 90 + h) | x <- [1, 2, 3]]
    ++ [initialBox (2850 + x * 20, 110 + h) | x <- [1, 2]]
    ++ [initialBox (3035 + x * 20, 70 + h) | x <- [1, 2, 3, 4]]
    ++ [initialBox (3055 + x * 20, 90 + h) | x <- [1, 2, 3]]
    ++ [initialBox (3075 + x * 20, 110 + h) | x <- [1, 2]]
    ++ [initialBox (3265 + x * 20, 70 + h) | x <- [1, 2, 3, 4]]
    ++ [initialBox (3265 + x * 20, 90 + h) | x <- [1, 2, 3]]
    ++ [initialBox (3265 + x * 20, 110 + h) | x <- [1, 2]]
    ++ [initialBox (3500 + x * 20, 50 + y * 20 + h) | x <- [1, 2], y <- [1, 2]]
    ++ [initialBox (3650 + x * 20, 120 + h) | x <- [1 .. 4]]
    ++ [initialBox (3900 + x * 20, 50 + y * 20 + h) | x <- [1, 2], y <- [1, 2]]
    ++ [initialBox (3950 + x * 20, 70 + h) | x <- [1 .. 9]]
    ++ [initialBox (3970 + x * 20, 90 + h) | x <- [1 .. 8]]
    ++ [initialBox (3990 + x * 20, 110 + h) | x <- [1 .. 7]]
    ++ [initialBox (4010 + x * 20, 130 + h) | x <- [1 .. 6]]
    ++ [initialBox (4030 + x * 20, 150 + h) | x <- [1 .. 5]]
    ++ [initialBox (4050 + x * 20, 170 + h) | x <- [1 .. 4]]
    ++ [initialBox (4070 + x * 20, 190 + h) | x <- [1 .. 3]]
    ++ [initialBox (4090 + x * 20, 210 + h) | x <- [1 .. 2]]

coinsLevel1 :: [Coin]
coinsLevel1 =
  [ initialCoin (200, 140 + h),
    initialCoin (310, 230 + h),
    initialCoin (1490, 140 + h),
    initialCoin (1650, 210 + h),
    initialCoin (1870, 140 + h),
    initialCoin (2160, 190 + h),
    initialCoin (3710, 140 + h)
  ]
    ++ [initialCoin (250 + x * 20, 140 + h) | x <- [1, 3, 5]]
    ++ [initialCoin (2100 + x * 20, 140 + h) | x <- [1, 3, 5]]
    ++ [initialCoin (2570 + x * 20, 210 + h) | x <- [1, 2]]

floorLevel1 :: [Box]
floorLevel1 =
  take
    35
    [ initialFloor
        ( (- fromIntegral width) / 2 + x * floorWidth,
          floorHeight / 2 + h
        )
      | x <- [0 .. 10] ++ [12, 13] ++ [15 .. 23] ++ [25 ..]
    ]

level2 :: LevelSettings
level2 = LevelSettings 20 monstersLevel2 coinsLevel2 boxesLevel2 floorLevel2 flagLevel2

flagLevel2 :: Flag
flagLevel2 = Flag 5200 (70 + h) False

monstersLevel2 :: [Monster]
monstersLevel2 =
  [ initialMonster (325, 70 + h) 200,
    Monster (100, 70 + h) (GoRight 93) 93 True,
    initialMonster (630, 70 + h) 25,
    initialMonster (1100, 70 + h) 150,
    Monster (710, 70 + h) (GoRight 200) 200 True,
    initialMonster (1770, 70 + h) 210,
    Monster (1270, 70 + h) (GoRight 200) 200 True,
    initialMonster (2280, 70 + h) 130,
    initialMonster (2260, 70 + h) 130,
    initialMonster (2580, 70 + h) 36,
    initialMonster (4620, 70 + h) 175
  ]

boxesLevel2 :: [Box]
boxesLevel2 =
  [initialBox (200 + x * 20, 120 + h) | x <- [1 .. 5]]
    ++ [ initialBox (350, 70 + h),
         initialBox (600, 150 + h),
         initialBox (840, 140 + h),
         initialBox (900, 140 + h)
       ]
    ++ [initialBox (390, 70 + y * 20 + h) | y <- [0, 1]]
    ++ [initialBox (430, 70 + y * 20 + h) | y <- [0 .. 2]]
    ++ [initialBox (470, 70 + y * 20 + h) | y <- [0 .. 3]]
    ++ [initialBox (510, 70 + y * 20 + h) | y <- [0 .. 3]]
    ++ [initialBox (550, 70 + y * 20 + h) | y <- [0 .. 2]]
    ++ [initialBox (650, 70 + y * 20 + h) | y <- [0 .. 2]]
    ++ [initialBox (690, 70 + y * 20 + h) | y <- [0, 1]]
    ++ [initialBox (800 + x * 20, 120 + h) | x <- [0 .. 2]]
    ++ [initialBox (840 + x * 20, 160 + h) | x <- [0 .. 3]]
    ++ [initialBox (900 + x * 20, 120 + h) | x <- [0 .. 2]]
    ++ [initialBox (1100 + x * 20, 120 + h + 20 * y) | x <- [0 .. 2], y <- [0 .. 5]]
    ++ [initialBox (1160 + x * 20, 90 + h + 20 * y) | x <- [0 .. 2], y <- [0 .. 2]]
    ++ [initialBox (1260 + x * 20, 130 + h) | x <- [0 .. 3]]
    ++ [initialBox (1340 + x * 20, 130 + h + 20 * y) | x <- [0 .. 2], y <- [0 .. 5]]
    ++ [initialBox (1500, 130 + y * 20 + h) | y <- [0 .. 5]]
    ++ [initialBox (1520, 130 + h)]
    ++ [initialBox (1540, 130 + h + y * 20) | y <- [0, 1]]
    ++ [initialBox (1600 + x * 20, 130 + h + 20 * y) | x <- [0 .. 2], y <- [0 .. 5]]
    ++ [initialBox (1720 + x * 20, 130 + h) | x <- [0 .. 2]]
    ++ [initialBox (1840 + x * 20, 160 + h + 20 * y) | x <- [0 .. 5], y <- [0 .. 1]]
    ++ [initialBox (2300 + x * 20, 70 + h + 20 * y) | x <- [0 .. 1], y <- [0 .. 2]]
    ++ [initialBox (2450 + x * 20, 70 + h + 20 * y) | x <- [0 .. 1], y <- [0 .. 3]]
    ++ [initialBox (2600 + x * 20, 70 + h + 20 * y) | x <- [0 .. 1], y <- [0 .. 1]]
    ++ [initialBox (3635 + x * 20, 70 + h) | x <- [0 .. 4]]
    ++ [initialBox (3655 + x * 20, 90 + h) | x <- [0 .. 3]]
    ++ [initialBox (3675 + x * 20, 110 + h) | x <- [0 .. 2]]
    ++ [initialBox (3695 + x * 20, 130 + h) | x <- [0 .. 1]]
    ++ [initialBox (3920 + x * 20, 50 + h) | x <- [0 .. 3]]
    ++ [initialBox (3900 + x * 20, 130 + h) | x <- [0 .. 3]]

coinsLevel2 :: [Coin]
coinsLevel2 =
  [initialCoin (220, 140 + h), initialCoin (600, 170 + h), initialCoin (820, 140 + h)] ++ [initialCoin (840 + x * 20, 180 + h) | x <- [0 .. 3]]
    ++ [initialCoin (1260 + x * 20, 150 + h) | x <- [0 .. 3]]
    ++ [initialCoin (1520, 150 + h)]
    ++ [initialCoin (1840 + x * 20, 200 + h) | x <- [0 .. 5]]
    ++ [initialCoin (3920 + x * 20, 70 + h) | x <- [0 .. 3]]

floorLevel2 :: [Box]
floorLevel2 =
  take
    35
    [ initialFloor
        ( (- fromIntegral width) / 2 + x * floorWidth,
          floorHeight / 2 + h
        )
      | x <- [0 .. 14] ++ [16 .. 21] ++ [23] ++ [25 .. 27] ++ [31 .. 33] ++ [35 ..]
    ]

level3 :: LevelSettings
level3 = LevelSettings 20 [] [] [] [] flagLevel3

flagLevel3 :: Flag
flagLevel3 = Flag 5000 (70 - fromIntegral height / 2) False
