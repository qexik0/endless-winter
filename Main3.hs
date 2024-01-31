module Main where

import Data.Fixed
import GHC.IO
import GHC.IO.Buffer (newBuffer)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

mainWindow :: Display
mainWindow = InWindow "Endless Winter" (800, 400) (100, 100)

animationSpeed :: Float
animationSpeed = 0.1

gravitationalConstant :: Float
gravitationalConstant = -400

runningSpriteFiles :: [String]
runningSpriteFiles =
  [ "assets/adventurer-run-00.bmp",
    "assets/adventurer-run-01.bmp",
    "assets/adventurer-run-02.bmp",
    "assets/adventurer-run-03.bmp",
    "assets/adventurer-run-04.bmp",
    "assets/adventurer-run-05.bmp"
  ]

jumpingSpriteFiles :: [String]
jumpingSpriteFiles =
  [ "assets/adventurer-jump-00.bmp",
    "assets/adventurer-jump-01.bmp",
    "assets/adventurer-jump-02.bmp",
    "assets/adventurer-jump-03.bmp"
  ]

fallingSpriteFiles :: [String]
fallingSpriteFiles =
  [ "assets/adventurer-fall-00.bmp",
    "assets/adventurer-fall-01.bmp"
  ]

parallaxLayers :: [(String, Float)]
parallaxLayers =
  [ ("assets/background/layer_6_far_buildings.bmp", 100)
  ]

runningSprites :: [Picture]
runningSprites = map (unsafePerformIO . loadBMP) runningSpriteFiles

jumpingSprites :: [Picture]
jumpingSprites = map (unsafePerformIO . loadBMP) jumpingSpriteFiles

fallingSprites :: [Picture]
fallingSprites = map (unsafePerformIO . loadBMP) fallingSpriteFiles

backgroundSprites :: [Picture]
backgroundSprites = map (unsafePerformIO . loadBMP . fst) parallaxLayers

data PlayerAnimation = Running | Jumping | Falling

data Platform = Platform
  { upperLeftPos :: (Float, Float),
    upperRightPos :: (Float, Float)
  }

data Game = Game
  { playerAnimation :: PlayerAnimation,
    animationFrame :: Int,
    animationTimeBuffer :: Float,
    playerPos :: (Float, Float),
    verticalSpeed :: Float,
    verticalAcceleration :: Float,
    backgroundPositions :: [Float],
    platforms :: [Platform]
  }

initialState :: Game
initialState =
  Game
    { playerAnimation = Running,
      animationFrame = 0,
      animationTimeBuffer = 0,
      playerPos = (-300, -100),
      verticalSpeed = 0,
      verticalAcceleration = 0,
      backgroundPositions = [0, 0],
      platforms =
        [ Platform
            { upperLeftPos = (-200, -200),
              upperRightPos = (-100, -200)
            }
        ]
    }

main :: IO ()
main = do
  play mainWindow white 60 initialState render handleEvents update

render :: Game -> Picture
render game =
  Pictures
    [ renderBackground game
    ]


renderBackground :: Game -> Picture
renderBackground game = Translate (head $ backgroundPositions game) 0 (Scale 1.5 1.5 (head backgroundSprites))

-- render :: Game -> Picture
-- render game =
--   Pictures
--     [ renderPlayer (playerPos game) (playerAnimation game) (animationFrame game)
--     ]

renderPlayer :: (Float, Float) -> PlayerAnimation -> Int -> Picture
renderPlayer (posX, posY) Running frame = Translate posX posY $ runningSprites !! (frame `mod` length runningSprites)
renderPlayer (posX, posY) Jumping frame = Translate posX posY $ jumpingSprites !! (frame `mod` length jumpingSprites)
renderPlayer (posX, posY) Falling frame = Translate posX posY $ fallingSprites !! (frame `mod` length fallingSprites)

handleEvents :: Event -> Game -> Game
handleEvents _ game = game

update :: Float -> Game -> Game
update dt game = updatePhysics dt (updateAnimations dt (updateBackground dt game))

updateBackground :: Float -> Game -> Game
updateBackground dt game = game {backgroundPositions = [head (backgroundPositions game) - snd (head parallaxLayers) * dt]}

updateAnimations :: Float -> Game -> Game
updateAnimations dt game =
  if curTime >= animationSpeed
    then game {animationFrame = animationFrame game + 1, animationTimeBuffer = newBuffer}
    else game {animationTimeBuffer = newBuffer}
  where
    curTime = animationTimeBuffer game + dt
    newBuffer = curTime `mod'` animationSpeed

updatePhysics :: Float -> Game -> Game
updatePhysics dt game =
  game
    { verticalAcceleration = updateVerticalAcceleration dt game,
      verticalSpeed = updateVerticalSpeed dt game,
      playerPos = updatePlayerPos dt game
    }

updateVerticalAcceleration :: Float -> Game -> Float
updateVerticalAcceleration dt game = verticalAcceleration game + dt * gravitationalConstant

updateVerticalSpeed :: Float -> Game -> Float
updateVerticalSpeed dt game = verticalSpeed game + dt * verticalAcceleration game

updatePlayerPos :: Float -> Game -> (Float, Float)
updatePlayerPos dt game = (fst (playerPos game), snd (playerPos game) + verticalSpeed game * dt)