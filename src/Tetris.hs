{--
Module      : Main
Description : Main module for running the Tetris game

Author      : Aatena Hasan, Theresa Van, Annie Wu
Class       : CS 4080 - Concepts of Programming Languages
Date        : 05/02/2019
--}

module Main where
import ConsoleGUI       -- stack install ansi-terminal
import Shapes

main = runGame tetrisGame

tetrisGame = Game { startGame     = start,
                    updateGame    = update,
                    drawGame      = draw,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris
                  }

-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]

type Vector = (Int,Int)

wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)
wellWidth = 10
wellHeight = 20

-- | The size of the game
gameSize :: (Int,Int)
gameSize = (gameWidth,gameHeight)
gameWidth = 10
gameHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (gameWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s

-- | Define a property that checks the following things:
-- that the falling shape in the game satisfies the Shape Invariant (prop_Shape),
-- that the size of the game is correct, i.e. equal to gameSize.
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (p, s) w r) = shapeProperty s && shapeSize w == wellSize

-- | Add black walls around a given shape
createWall:: Int -> Row
createWall l = replicate l (Just Black)

addWall:: Shape -> Shape
addWall(S (s:ss)) = S $ (s:ss) ++ [createWall $ length s]

addWalls :: Shape -> Shape
addWalls s = iterate(rotate90 . addWall) s !! 4

-- | Create visual representation of the current game state that is presented to the user / player

draw :: Tetris -> Shape
draw (Tetris (v,p) w _) = addWalls $ combineShapes (shiftShape v p) w

chooseShape :: [Double] -> [Int]
chooseShape s = map(\x -> floor (6.99*x)) s

start :: [Double] -> Tetris
start rs = Tetris (startPosition, shape1) (emptyShape wellSize) supply
  where
    shape1:supply = zipWith (!!) (repeat allShapes) (chooseShape rs)

-- update reacts to user input and moves the piece accordingly

update :: Action -> Tetris -> Maybe (Int,Tetris)
update MoveDown t = tick t
update MoveLeft t = Just (0, movePiece (-1) t)
update MoveRight t = Just(0, movePiece 1 t)
update Rotate t = Just (0, rotatePiece t)
update Slam t = Just (0, slamPiece 1 t)
update _ t = tick t

-- | Move the fallen piece
move :: Vector -> Tetris -> Tetris
move v1 (Tetris (v2, p) w r) = Tetris (vAdd v1 v2, p) w r

-- | Handle the Tick action that can be called from update

tick :: Tetris -> Maybe (Int,Tetris)
tick t
  | collision updateGame = dropNewPiece t
  | otherwise = Just (0, move (0, 1) t)
    where updateGame = move (0, 1) t


slamPiece :: Int -> Tetris -> Tetris
slamPiece s t
  | collision $ slam s = move(0, s-1) t
  | otherwise = slamPiece(s+1)t
    where slam x = move(0,x) t

-- | Test if the falling piece has collided with the walls or something in the game
collision :: Tetris -> Bool
collision (Tetris ((x, y), p) w r)
  | x < 0 = True
  | (x + width) > wellWidth = True
  | (y + height) > wellHeight = True
  | overlaps (place ((x, y), p)) w = True
  | otherwise = False
    where (width, height) = shapeSize p

-- | Handle MoveLeft and MoveRight to be called from update
movePiece :: Int -> Tetris -> Tetris
movePiece dir t
  | collision nv = t
  | otherwise = nv
    where nv = move(dir, 0)t

-- | Rotate the falling piece
rotate :: Tetris -> Tetris
rotate (Tetris (v, p) w r) = Tetris (v, rotate90 p) w r

-- | When a tall and narrow piece is rotated, it becomes wider.
adjust :: Tetris -> Tetris
adjust (Tetris ((x, y), p) w r)
  | (x + width) > wellWidth = (Tetris ((wellWidth-width, y), p) w r)
  | (y + height) > wellHeight = (Tetris (( x, y-(height-1)), p) w r)
  | otherwise = (Tetris ((x, y), p) w r)
    where (width, height) = shapeSize p

-- | handle Rotate action to be called from update
rotatePiece :: Tetris -> Tetris
rotatePiece t
  | collision rot = adjust rot
  | otherwise = rot
    where rot = rotate t

dropNewPiece :: Tetris -> Maybe (Int,Tetris)
dropNewPiece (Tetris (v, p) w (r:rs))
  | overlaps nw (place np) = Nothing
  | otherwise = Just (sc, (Tetris np nw rs))
    where (sc, nw) = clearLines $ combineShapes (place (v, p)) w
          np = (startPosition, r)


isComplete :: Row -> Bool
isComplete = all (/= Nothing)

-- | Remove completed lines
clearLines :: Shape -> (Int,Shape)
clearLines (S r) = (clr, shiftShape (0, clr) $ S (filter (notComplete) r))
  where clr = length $ filter (isComplete) r
        notComplete x = not $ isComplete x
