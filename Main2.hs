module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
  let window = InWindow "Infinite Runner" (800, 600) (100, 100)
  
  -- Load the image
  sprite <- loadBMP "assets/adventurer-idle-00.bmp"
  
  -- Pass to the play function
  play window white 60 initialState (render sprite) handleEvent update

fallSpeed :: Float
fallSpeed = -600

-- height of the player
heightPlayer :: Float
heightPlayer = 10

-- height of the platform
platformHeight :: Float
platformHeight = -100

-- radius of the player
radiusPlayer :: Float
radiusPlayer = 20

-- initial position of the player
posXPlayer :: Float
posXPlayer = -350 

-- Speed of the game aka speed of the obstacles
gameSpeed :: Float
gameSpeed = -50

data Game = Game {
    playerHeight :: Float,
    velocity :: Float,
    jumpRequest :: Bool,
    inAir :: Bool,
    doubleJumpUsed :: Bool
}

initialState :: Game
initialState = Game {
    playerHeight = 0,
    velocity = 0,
    jumpRequest = False,
    inAir = False,
    doubleJumpUsed = False
}

jumpVelocity :: Float
jumpVelocity = 400


-- Upward acceleration when the jump button is pressed
jumpAccel :: Float
jumpAccel = 2500

-- Modify your render function to accept the Picture as an argument
render :: Picture -> Game -> Picture
render sprite game = Pictures 
  [ Translate posXPlayer (playerHeight game) $ sprite
  , Color (greyN 0.5) $ Polygon [(posXPlayer - 60,platformHeight), (posXPlayer - 60, platformHeight - 20), (posXPlayer + 60, platformHeight - 20),(posXPlayer + 60, platformHeight)]
  ]

handleEvent :: Event -> Game -> Game
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) game
  | not (inAir game) || not (doubleJumpUsed game) =
    game { jumpRequest = True, doubleJumpUsed = inAir game }
handleEvent _ game = game

update :: Float -> Game -> Game
update deltaTime game
  | jumpRequest game =
    game { inAir = True, jumpRequest = False, velocity = jumpVelocity, playerHeight = newHeightJump }
  | playerHeight game > platformHeight =
    game { inAir = True, velocity = newVelocity, playerHeight = newHeight }
  | otherwise = game { inAir = False, velocity = 0, playerHeight = platformHeight, doubleJumpUsed = False }
  where
    newVelocity = velocity game + fallSpeed * deltaTime
    newHeight = playerHeight game + newVelocity * deltaTime
    newHeightJump = playerHeight game + jumpVelocity * deltaTime