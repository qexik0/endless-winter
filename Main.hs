module Main where

import Control.Concurrent (forkIO, forkOS)
import Control.Monad (unless)
import Data.Fixed (mod')
import Debug.Trace
import GHC.Conc (threadDelay)
import GHC.IO
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified SDL
import qualified SDL.Mixer as Mix
import System.Process (readProcess)
import System.Random (randomRIO)

-- The width of the game window.
screenWidth :: Float
screenWidth = 800

-- The height of the game window.
screenHeight :: Float
screenHeight = 400

-- The width of a parallax layer.
parallaxWidth :: Float
parallaxWidth = 723

-- The height of a parallax layer.
parallaxHeight :: Float
parallaxHeight = 256

-- Scaling factor used to adjust the parallax layers to the screen height.
parallaxScalingFactor :: Float
parallaxScalingFactor = screenHeight / parallaxHeight

-- The scaled width of a parallax layer.
scaledLayerWidth :: Float
scaledLayerWidth = parallaxScalingFactor * parallaxWidth

-- Speed of the animation frames.
animationSpeed :: Float
animationSpeed = 0.1

-- Initial X position of the player.
playerXPos :: Float
playerXPos = -300

-- Y position of the ground in the game.
groundYPos :: Float
groundYPos = -130

-- Speed of the barrels in the game.
barrelSpeed :: Float
barrelSpeed = 400

-- Minimum distance between barrels.
minBarrelDistance :: Float
minBarrelDistance = 250

-- Maximum distance between barrels.
maxBarrelDistance :: Float
maxBarrelDistance = 1000

-- The main window display configuration.
mainWindow :: Display
mainWindow = InWindow "Endless Winter" (800, 400) (100, 100)

-- Possible states of the player's jump.
data JumpState = NoneJump | SingleJump | DoubleJump deriving (Eq)

-- Possible states of the game.
data GameState = Initial | Playing | Over deriving (Eq)

-- Represents a barrel in the game with stacking information and position.
data Barrel = Barrel
  { isStacked :: Bool,  -- Indicates whether the barrel is stacked or not.
    position :: Float   -- The horizontal position of the barrel.
  }

-- Represents the game state with various attributes.
data Game = Game
  { backgroundPositions :: [Float],      -- Positions of parallax background layers.
    playerAnimation :: PlayerAnimation,  -- Current animation state of the player.
    animationFrame :: Int,               -- Current frame of the player animation.
    animationTimeBuffer :: Float,        -- Time buffer for animation updates.
    playerYPos :: Float,                 -- Vertical position of the player.
    jumpState :: JumpState,              -- Current state of the player's jump.
    verticalVelocity :: Float,           -- Vertical velocity of the player.
    barrels :: [Barrel],                 -- List of barrels in the game.
    gameState :: GameState,              -- Current state of the game.
    score :: Float,                      -- Current score in the game.
    highScore :: Float                   -- Highest score achieved.
  }

-- Represents the initial state of the game.
initialState :: Game
initialState =
  Game
    { backgroundPositions = replicate (length parallaxLayers) 0,  -- Initialize background positions.
      playerAnimation = Running,                                  -- Set player animation to Running.
      animationFrame = 0,                                         -- Set initial animation frame.
      animationTimeBuffer = 0,                                    -- Initialize animation time buffer.
      playerYPos = groundYPos + 500,                              -- Set initial vertical player position.
      jumpState = NoneJump,                                       -- Set initial jump state.
      verticalVelocity = 0,                                       -- Initialize vertical velocity.
      barrels = [Barrel {isStacked = True, position = 600}],     -- Initialize barrels with a stacked barrel.
      gameState = Initial,                                        -- Set initial game state.
      score = 0,                                                  -- Initialize score.
      highScore = 0                                               -- Initialize high score.
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

-- Represents the sprite for a barrel.
barrelSprite :: Picture
barrelSprite = (Scale 3 3 . unsafePerformIO . loadBMP) "assets/barrel.bmp"

-- Represents the sprite for the menu.
menuSprite :: Picture
menuSprite = unsafePerformIO . loadBMP $ "assets/menu.bmp"

-- Represents the sprite for the game over screen.
gameOverScreen :: Picture
gameOverScreen = unsafePerformIO . loadBMP $ "assets/dead.bmp"

-- Gravitational constant used for player physics.
gravitationalConstant :: Float
gravitationalConstant = -1000

-- Initial velocity applied when the player jumps.
initialJumpVelocity :: Float
initialJumpVelocity = 500

-- List of running animation sprites for the player.
runningSprites :: [Picture]
runningSprites = map (Scale 3 3 . unsafePerformIO . loadBMP) runningSpriteFiles

-- List of jumping animation sprites for the player.
jumpingSprites :: [Picture]
jumpingSprites = map (Scale 3 3 . unsafePerformIO . loadBMP) jumpingSpriteFiles

-- List of falling animation sprites for the player.
fallingSprites :: [Picture]
fallingSprites = map (Scale 3 3 . unsafePerformIO . loadBMP) fallingSpriteFiles

-- List of background layer sprites loaded from image files.
backgroundSprites :: [Picture]
backgroundSprites = map (unsafePerformIO . loadBMP . fst) parallaxLayers

-- Delta value used for horizontal collision detection.
horizontalCollisionDelta :: Float
horizontalCollisionDelta = 20

-- Vertical collision position for a single barrel.
singleBarrelVerticalCollsion :: Float
singleBarrelVerticalCollsion = groundYPos + 45

-- Vertical collision position for a stacked barrel.
doubleBarrelVerticalCollsion :: Float
doubleBarrelVerticalCollsion = groundYPos + 90

-- Represents different player animation states.
data PlayerAnimation = Running | Jumping | Falling

-- Thread for playing background music.
musicThread :: IO ()
musicThread = do
  SDL.initialize [SDL.InitAudio]
  Mix.openAudio Mix.defaultAudio 256
  music <- Mix.load "assets/music.ogg"
  Mix.playMusic Mix.Forever music

-- Main function responsible for launching the game.
main :: IO ()
main = do
  _ <- forkOS musicThread  -- Forks a separate thread for playing background music.
  play mainWindow white 60 initialState render handleEvents update  -- Initiates the Gloss game loop.

-- Rendering function that transforms the game state into a visual representation.
render :: Game -> Picture
render game
  | gameState game == Playing =
      Pictures
        [ renderBackground game,                   -- Render background layers.
          renderBarrels (barrels game),            -- Render barrels.
          renderPlayer (playerAnimation game) (animationFrame game) (playerYPos game),  -- Render player.
          renderScore (score game) (highScore game)  -- Render score.
        ]
  | gameState game == Over =
      Pictures
        [ renderBackground game,                   -- Render background layers.
          renderBarrels (barrels game),            -- Render barrels.
          gameOverScreen                          -- Render game over screen.
        ]
  | otherwise =
      Pictures
        [ renderBackground game,                   -- Render background layers.
          renderBarrels (barrels game),            -- Render barrels.
          menuSprite                              -- Render menu.
        ]

-- Rendering function for barrels.
renderBarrels :: [Barrel] -> Picture
renderBarrels barrels = Pictures $ map renderBarrel barrels

-- Rendering function for the player's score.
renderScore :: Float -> Float -> Picture
renderScore score hightScore =
  Pictures
    [ translate 250 170 $ scale 0.12 0.12 $ color red $ text ("Score: " ++ show (round score)),
      translate 250 140 $ scale 0.12 0.12 $ color red $ text ("High score: " ++ show (round hightScore))
    ]

-- Rendering function for an individual barrel.
renderBarrel :: Barrel -> Picture
renderBarrel barrel
  | isStacked barrel = Pictures [renderBarrel (barrel {isStacked = False}), Translate (position barrel) (groundYPos + 40) barrelSprite]
  | otherwise = Translate (position barrel) (groundYPos - 15) barrelSprite

-- Rendering function for a parallax layer.
renderLayer :: Float -> Picture -> Picture
renderLayer position layer =
  Pictures
    [ translateHorizontal position scaledLayer,                         -- Render the first layer at the given position.
      translateHorizontal (position + scaledLayerWidth) scaledLayer    -- Render the second layer adjacent to the first one.
    ]
  where
    scaledLayer = Scale parallaxScalingFactor parallaxScalingFactor layer

    -- Helper function to translate a picture horizontally.
    translateHorizontal :: Float -> Picture -> Picture
    translateHorizontal x = Translate x 0

-- Renders the parallax background layers based on the game state.
renderBackground :: Game -> Picture
renderBackground game = Pictures (zipWith renderLayer (backgroundPositions game) backgroundSprites)

-- Renders the player based on animation state, frame, and position.
renderPlayer :: PlayerAnimation -> Int -> Float -> Picture
renderPlayer Running frame playerYPos = basePlayerRender playerYPos $ runningSprites !! frame
renderPlayer Jumping frame playerYPos = basePlayerRender playerYPos $ jumpingSprites !! frame
renderPlayer Falling frame playerYPos = basePlayerRender playerYPos $ fallingSprites !! frame

-- Renders the player at a given position using translation.
basePlayerRender :: Float -> Picture -> Picture
basePlayerRender = Translate playerXPos

-- Handles events, updating the game state accordingly.
handleEvents :: Event -> Game -> Game
handleEvents (EventKey (SpecialKey KeySpace) Down _ _) game
  | jumpState game == NoneJump =
      game
        { verticalVelocity = initialJumpVelocity,  -- Apply initial jump velocity.
          jumpState = SingleJump
        }
  | jumpState game == SingleJump =
      game
        { verticalVelocity = initialJumpVelocity,  -- Apply initial jump velocity for double jump.
          jumpState = DoubleJump
        }
  | otherwise = game
handleEvents (EventKey (SpecialKey KeyEnter) Down _ _) game
  | gameState game == Playing = game  -- Continue playing when Enter key is pressed during the game.
  | otherwise = initialState {gameState = Playing, highScore = highScore game}  -- Start a new game when Enter key is pressed on the menu.
handleEvents _ game = game

-- Updates the game state based on elapsed time and current state.
update :: Float -> Game -> Game
update dt game
  | gameState game == Playing =
      updateScore
        dt
        ( updateBarrels
            dt
            ( checkCollision
                dt
                ( updatePlayerPhysics
                    dt
                    ( checkAnimationState
                        (playerAnimation game)
                        (updateAnimations (playerAnimation game) dt (updateBackground dt game))
                    )
                )
            )
        )
  | otherwise = game

-- Updates the player's score based on elapsed time.
updateScore :: Float -> Game -> Game
updateScore dt game = game {score = newScore, highScore = max (highScore game) newScore}
  where
    newScore = score game + 20 * dt

-- Checks for collision between the player and a single barrel.
checkSingleBarrelCollision :: Float -> Game -> Barrel -> Bool
checkSingleBarrelCollision dt game barrel
  | (abs (position barrel - playerXPos) < horizontalCollisionDelta)
      && (playerYPos game < singleBarrelVerticalCollsion)
      && dt > 0 =
      trace "There was a collision" True
  | otherwise = False

-- Checks for collision between the player and a stacked barrel.
checkStackedBarrelCollision :: Float -> Game -> Barrel -> Bool
checkStackedBarrelCollision dt game barrel
  | (abs (position barrel - playerXPos) < horizontalCollisionDelta)
      && (playerYPos game < doubleBarrelVerticalCollsion)
      && dt > 0 =
      trace "There was a collision" True
  | otherwise = False

-- Checks for collision between the player and a barrel, based on whether it is stacked or not.
checkBarrelCollision :: Float -> Game -> Barrel -> Bool
checkBarrelCollision dt game barrel
  | isStacked barrel = checkStackedBarrelCollision dt game barrel
  | otherwise = checkSingleBarrelCollision dt game barrel

-- Checks for collision with any barrel and updates the game state accordingly.
checkCollision :: Float -> Game -> Game
checkCollision dt game
  | null (barrels game) = game
  | otherwise = if any (checkBarrelCollision dt game) (barrels game) then game {gameState = Over} else game {gameState = Playing}

-- Removes barrels that have moved past the left side of the screen.
clearPastBarrels :: Game -> Game
clearPastBarrels game =
  game
    { barrels = dropWhile (\x -> position x + 100 < -screenWidth / 2) (barrels game)
    }

-- Updates the positions and generates new barrels based on the elapsed time.
updateBarrels :: Float -> Game -> Game
updateBarrels dt game =
  game
    { barrels = if shouldGenerate then generateBarrels updatedBarrels else updatedBarrels
    }
  where
    shouldGenerate = null (barrels game) || position (last (barrels game)) <= screenWidth
    updatedBarrels = map (updateBarrel dt) (barrels (clearPastBarrels game))

-- Generates a random float within a given range.
randomInRange :: Float -> Float -> Float
randomInRange left right = unsafePerformIO $ randomRIO (left, right)

-- Generates a new barrel at a random position within a given range.
generateBarrels :: [Barrel] -> [Barrel]
generateBarrels [] = [generateBarrel (screenWidth / 2 + 50) screenWidth]
generateBarrels barrels = barrels ++ [generateBarrel (position (last barrels) + minBarrelDistance) (position (last barrels) + maxBarrelDistance)]

-- Generates a barrel at a random position within a given range.
generateBarrel :: Float -> Float -> Barrel
generateBarrel left right = Barrel {isStacked = stacked, position = randomInRange left right}
  where
    newPos = randomInRange left right
    stacked = (newPos - left) <= ((right - left) / 2)

-- Updates the position of a barrel based on elapsed time and speed.
updateBarrel :: Float -> Barrel -> Barrel
updateBarrel dt barrel = barrel {position = position barrel - dt * barrelSpeed}

-- Updates the player's vertical velocity and position based on physics.
updatePlayerPhysics :: Float -> Game -> Game
updatePlayerPhysics dt game =
  game
    { verticalVelocity = if didHitTheGround then 0 else newVelocity,  -- Stop vertical motion if hitting the ground.
      playerYPos = if didHitTheGround then groundYPos else newPlayerYPos,  -- Keep player on the ground.
      jumpState = if didHitTheGround then NoneJump else jumpState game  -- Reset jump state if hitting the ground.
    }
  where
    newVelocity = verticalVelocity game + dt * gravitationalConstant  -- Apply gravity.
    newPlayerYPos = playerYPos game + dt * newVelocity  -- Update player's vertical position.
    didHitTheGround = newPlayerYPos <= groundYPos  -- Check if player has hit the ground.


-- Updates the animation state and time buffer for Running, Jumping, and Falling animations.
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

-- Checks the animation state based on the player's vertical position and velocity.
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

-- Updates the animation time buffer based on elapsed time and animation speed.
updateAnimationTime :: Float -> Float -> Float
updateAnimationTime dt curBuf = (curBuf + dt) `mod'` animationSpeed

-- Increments the animation frame if the time buffer exceeds the animation speed.
incrementFrameIf :: Float -> Float -> Int -> Int -> Int
incrementFrameIf dt curBuf curFrame modulo
  | dt + curBuf >= animationSpeed = (curFrame + 1) `mod` modulo
  | otherwise = curFrame

-- Updates the position of a parallax layer based on elapsed time and speed.
updateLayerPosition :: Float -> Float -> Float -> Float
updateLayerPosition dt pos speed
  | rightmostPoint < -screenWidth / 2 = newPos + scaledLayerWidth
  | otherwise = newPos
  where
    newPos = pos - speed * dt
    rightmostPoint = newPos + scaledLayerWidth / 2

-- Updates the positions of all background layers based on elapsed time.
updateBackground :: Float -> Game -> Game
updateBackground dt game = game {backgroundPositions = zipWith (updateLayerPosition dt) (backgroundPositions game) (map snd parallaxLayers)}
