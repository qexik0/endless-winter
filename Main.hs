module Main where

import Data.Fixed (mod')
import GHC.IO
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Process (readProcess)
import System.Random (randomRIO)

screenWidth :: Float
screenWidth = 800

screenHeight :: Float
screenHeight = 400

parallaxWidth :: Float
parallaxWidth = 723

parallaxHeight :: Float
parallaxHeight = 256

parallaxScalingFactor :: Float
parallaxScalingFactor = screenHeight / parallaxHeight

scaledLayerWidth :: Float
scaledLayerWidth = parallaxScalingFactor * parallaxWidth

animationSpeed :: Float
animationSpeed = 0.1

playerXPos :: Float
playerXPos = -300

groundYPos :: Float
groundYPos = -130

barrelSpeed :: Float
barrelSpeed = 400

minBarrelDistance :: Float
minBarrelDistance = 250

maxBarrelDistance :: Float
maxBarrelDistance = 1000

mainWindow :: Display
mainWindow = InWindow "Endless Winter" (800, 400) (100, 100)

data JumpState = NoneJump | SingleJump | DoubleJump deriving (Eq)

data Barrel = Barrel
  { isStacked :: Bool,
    position :: Float
  }

data Game = Game
  { backgroundPositions :: [Float],
    playerAnimation :: PlayerAnimation,
    animationFrame :: Int,
    animationTimeBuffer :: Float,
    playerYPos :: Float,
    jumpState :: JumpState,
    verticalVelocity :: Float,
    barrels :: [Barrel]
  }

initialState :: Game
initialState =
  Game
    { backgroundPositions = replicate (length parallaxLayers) 0,
      playerAnimation = Running,
      animationFrame = 0,
      animationTimeBuffer = 0,
      playerYPos = groundYPos,
      jumpState = NoneJump,
      verticalVelocity = 0,
      barrels = [Barrel {isStacked = True, position = 600}]
    }

parallaxLayers :: [(String, Float)]
parallaxLayers =
  [ ("assets/background/layer_1_ground.bmp", 400),
    ("assets/background/layer_2_stars.bmp", 10),
    ("assets/background/layer_3_moon.bmp", 40),
    ("assets/background/layer_4_clouds_1.bmp", 70),
    ("assets/background/layer_5_clouds_2.bmp", 80),
    ("assets/background/layer_6_far_buildings.bmp", 150),
    ("assets/background/layer_7_bg_buildings.bmp", 250),
    ("assets/background/layer_8_fg_buildings.bmp", 300),
    ("assets/background/layer_9_wall.bmp", 360)
  ]

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

barrelSprite :: Picture
barrelSprite = (Scale 3 3 . unsafePerformIO . loadBMP) "assets/barrel.bmp"

gravitationalConstant :: Float
gravitationalConstant = -1000

initialJumpVelocity :: Float
initialJumpVelocity = 500

runningSprites :: [Picture]
runningSprites = map (Scale 3 3 . unsafePerformIO . loadBMP) runningSpriteFiles

jumpingSprites :: [Picture]
jumpingSprites = map (Scale 3 3 . unsafePerformIO . loadBMP) jumpingSpriteFiles

fallingSprites :: [Picture]
fallingSprites = map (Scale 3 3 . unsafePerformIO . loadBMP) fallingSpriteFiles

backgroundSprites :: [Picture]
backgroundSprites = map (unsafePerformIO . loadBMP . fst) parallaxLayers

data PlayerAnimation = Running | Jumping | Falling

main :: IO ()
main = do
  play mainWindow white 60 initialState render handleEvents update

render :: Game -> Picture
render game =
  Pictures
    [ renderBackground game,
      renderPlayer (playerAnimation game) (animationFrame game) (playerYPos game),
      renderBarrels (barrels game)
    ]

renderBarrels :: [Barrel] -> Picture
renderBarrels barrels = Pictures $ map renderBarrel barrels

renderBarrel :: Barrel -> Picture
renderBarrel barrel
  | isStacked barrel = Pictures [renderBarrel (barrel {isStacked = False}), Translate (position barrel) (groundYPos + 40) barrelSprite]
  | otherwise = Translate (position barrel) (groundYPos - 15) barrelSprite

renderLayer :: Float -> Picture -> Picture
renderLayer position layer =
  Pictures
    [ translateHorizontal position scaledLayer,
      translateHorizontal (position + scaledLayerWidth) scaledLayer
    ]
  where
    scaledLayer = Scale parallaxScalingFactor parallaxScalingFactor layer

    translateHorizontal :: Float -> Picture -> Picture
    translateHorizontal x = Translate x 0

renderBackground :: Game -> Picture
renderBackground game = Pictures (zipWith renderLayer (backgroundPositions game) backgroundSprites)

renderPlayer :: PlayerAnimation -> Int -> Float -> Picture
renderPlayer Running frame playerYPos = basePlayerRender playerYPos $ runningSprites !! frame
renderPlayer Jumping frame playerYPos = basePlayerRender playerYPos $ jumpingSprites !! frame
renderPlayer Falling frame playerYPos = basePlayerRender playerYPos $ fallingSprites !! frame

basePlayerRender :: Float -> Picture -> Picture
basePlayerRender = Translate playerXPos

handleEvents :: Event -> Game -> Game
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) game
  | jumpState game == NoneJump =
      game
        { verticalVelocity = initialJumpVelocity,
          jumpState = SingleJump
        }
  | jumpState game == SingleJump =
      game
        { verticalVelocity = initialJumpVelocity,
          jumpState = DoubleJump
        }
  | otherwise = game
handleEvents _ game = game

update :: Float -> Game -> Game
update dt game =
  updateBarrels
    dt
    ( updatePlayerPhysics
        dt
        ( checkAnimationState
            (playerAnimation game)
            (updateAnimations (playerAnimation game) dt (updateBackground dt game))
        )
    )

clearPastBarrels :: Game -> Game
clearPastBarrels game = game {
  barrels = dropWhile (\x -> position x + 100 < -screenWidth / 2) (barrels game)
}

updateBarrels :: Float -> Game -> Game
updateBarrels dt game =
  game
    { barrels = if shouldGenerate then generateBarrels updatedBarrels else updatedBarrels
    }
  where
    shouldGenerate = null (barrels game) || position (last (barrels game)) <= screenWidth
    updatedBarrels = map (updateBarrel dt) (barrels (clearPastBarrels game))

randomInRange :: Float -> Float -> Float
randomInRange left right = unsafePerformIO $ randomRIO (left, right)

generateBarrels :: [Barrel] -> [Barrel]
generateBarrels [] = [generateBarrel (screenWidth / 2 + 50) screenWidth]
generateBarrels barrels = barrels ++ [generateBarrel (position (last barrels) + minBarrelDistance) (position (last barrels) + maxBarrelDistance)]

generateBarrel :: Float -> Float -> Barrel
generateBarrel left right = Barrel {isStacked = stacked, position = randomInRange left right}
  where
    newPos = randomInRange left right
    stacked = (newPos - left) <= ((right - left) / 2)

updateBarrel :: Float -> Barrel -> Barrel
updateBarrel dt barrel = barrel {position = position barrel - dt * barrelSpeed}

updatePlayerPhysics :: Float -> Game -> Game
updatePlayerPhysics dt game =
  game
    { verticalVelocity = if didHitTheGround then 0 else newVelocity,
      playerYPos = if didHitTheGround then groundYPos else newPlayerYPos,
      jumpState = if didHitTheGround then NoneJump else jumpState game
    }
  where
    newVelocity = verticalVelocity game + dt * gravitationalConstant
    newPlayerYPos = playerYPos game + dt * newVelocity
    didHitTheGround = newPlayerYPos <= groundYPos

updateAnimations :: PlayerAnimation -> Float -> Game -> Game
updateAnimations Running dt game =
  game
    { animationFrame = incrementFrameIf dt (animationTimeBuffer game) (animationFrame game) (length runningSprites),
      animationTimeBuffer = updateAnimationTime dt (animationTimeBuffer game)
    }
updateAnimations Jumping dt game =
  game
    { animationFrame = incrementFrameIf dt (animationTimeBuffer game) (animationFrame game) (length jumpingSprites),
      animationTimeBuffer = updateAnimationTime dt (animationTimeBuffer game)
    }
updateAnimations Falling dt game =
  game
    { animationFrame = incrementFrameIf dt (animationTimeBuffer game) (animationFrame game) (length fallingSprites),
      animationTimeBuffer = updateAnimationTime dt (animationTimeBuffer game)
    }

checkAnimationState :: PlayerAnimation -> Game -> Game
checkAnimationState Running game =
  game
    { playerAnimation = if (playerYPos game) <= groundYPos then Running else if (verticalVelocity game) > 0 then Jumping else Falling,
      animationFrame = if (playerYPos game) > groundYPos then 0 else (animationFrame game)
    }
checkAnimationState Jumping game =
  game
    { playerAnimation = if (playerYPos game) <= groundYPos then Running else if (verticalVelocity game) > 0 then Jumping else Falling,
      animationFrame = if verticalVelocity game < 0 then 0 else (animationFrame game)
    }
checkAnimationState Falling game =
  game
    { playerAnimation = if (playerYPos game) <= groundYPos then Running else if (verticalVelocity game) > 0 then Jumping else Falling,
      animationFrame = if groundYPos == playerYPos game then 0 else (animationFrame game)
    }

updateAnimationTime :: Float -> Float -> Float
updateAnimationTime dt curBuf = (curBuf + dt) `mod'` animationSpeed

incrementFrameIf :: Float -> Float -> Int -> Int -> Int
incrementFrameIf dt curBuf curFrame modulo
  | dt + curBuf >= animationSpeed = (curFrame + 1) `mod` modulo
  | otherwise = curFrame

updateLayerPosition :: Float -> Float -> Float -> Float
updateLayerPosition dt pos speed
  | rightmostPoint < -screenWidth / 2 = newPos + scaledLayerWidth
  | otherwise = newPos
  where
    newPos = pos - speed * dt
    rightmostPoint = newPos + scaledLayerWidth / 2

updateBackground :: Float -> Game -> Game
updateBackground dt game = game {backgroundPositions = zipWith (updateLayerPosition dt) (backgroundPositions game) (map snd parallaxLayers)}