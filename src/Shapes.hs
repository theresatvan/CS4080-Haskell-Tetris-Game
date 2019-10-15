{--
Module      : Shapes
Description : Functions and types for the 7 Tetris pieces

Author      : Aatena Hasan, Theresa Van, Annie Wu
Class       : CS 4080 - Concepts of Programming Languages
Date        : 05/02/2019
--}


module Shapes where
import Data.List(transpose)
import Data.Maybe(isNothing)
import Test.QuickCheck

type Square = Maybe Color

data Color = Black | Red | Orange | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is a list of lists of squares.
-- | A square can be empty or filled with a color block.

data Shape = S [Row] deriving (Eq)

type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- | Show the shape
showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
    where
        showRow :: Row -> String
        showRow r = [showSquare s | s <- r]

        showSquare Nothing = '.'
        showSquare (Just Black) = '#' -- can change to '█' on linux/mac
        showSquare (Just Grey)  = 'g' -- can change to '▓'
        showSquare (Just c)     = head (show c)

instance Show Shape where
    show = showShape
    showList ss r = unlines (map show ss) ++ r

-- | The 7 shapes (tetrominos) used in the Tetris game
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes] 
    where
        makeSquares = map (map color)
        color c = lookup c [('I',Red),('J',Orange),('T',Yellow),('O',Green), ('Z',Blue),('L',Cyan),('S',Purple)]
        shapes = [["I", "I", "I", "I"],
                 [" J", " J", "JJ"],
                 [" T", "TT", " T"],
                 ["OO", "OO"],
                 [" Z", "ZZ", "Z "],
                 ["LL", " L", " L"],
                 ["S ", "SS", " S"]]

-- | Return an empty shape of a given size
emptyShape :: (Int,Int) -> Shape
emptyShape (x,y) = S (replicate y $ replicate x Nothing)

-- | Return the size (width and height) of a shape, i.e. number of columns and rows
shapeSize :: Shape -> (Int,Int)
shapeSize (S s) = (length $ s !! 0, length s)

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount (S s) = length [c | c <- concat s, c /= Nothing]

-- | Define the property that all values of type Shape are expected to:
-- have at least one row, have at least one column, and be rectangular
shapeProperty :: Shape -> Bool
shapeProperty (S s)
  |  fst size * snd size == 0 = False
  |  otherwise = all (== fst size) [length(c) | (c) <- s]
     where size = shapeSize (S s)

-- | Random color generator
randomColor :: Gen Color
randomColor = elements $ enumFromTo Black Grey

instance Arbitrary Color where
    arbitrary = randomColor

-- | Random shape generator
randomShape :: Gen Shape
randomShape = elements allShapes

instance Arbitrary Shape where
    arbitrary = randomShape

-- | Rotate a shape 90 degrees clockwise or counterclockwise
rotate90 :: Shape -> Shape
rotate90 (S s) = S $ transpose $ reverse s

-- | Shift a shape right and down (i.e. adds empty squares by the top and left edges)
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (l,t) s = shiftTop t (shiftLeft l s)

shiftLeft :: Int -> Shape -> Shape
shiftLeft l (S s) = S $ zipWith (++) emptyGrid s
    where emptyGrid = rows $ emptyShape (l, (length s))

shiftTop :: Int -> Shape -> Shape
shiftTop t (S s) = S $ emptyGrid ++ s
    where emptyGrid = rows $ emptyShape (length (s !! 0), t)

-- | Add padding to a shape (i.e. add empty squares by the bottom and right edge)
padShape :: (Int,Int) -> Shape -> Shape
padShape (r,b) s = rotate90 $ rotate90 $ shiftShape (r,b) (rotate90 $ rotate90(s))

padRight :: Int -> Shape -> Shape
padRight r (S s) = S $ zipWith (++) s emptyGrid
    where emptyGrid = rows $ emptyShape (r, (length s))

padBottom :: Int -> Shape -> Shape
padBottom b (S s) = S $ s ++ emptyGrid
    where emptyGrid = rows $ emptyShape (length (s !! 0), b)

-- | Pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (w,h) s = padShape ((w - (fst size)),(h - (snd size))) s
    where size = shapeSize s

-- | Test if two shapes overlap (i.e. if they have non empty squares in the same position)
rowsOverlap :: Row -> Row -> Bool
rowsOverlap _ [] = False
rowsOverlap [] _ = False
rowsOverlap row1 row2 = any (==True) $
    zipWith (\x y -> x /= Nothing && y/= Nothing) row1 row2

overlaps :: Shape -> Shape -> Bool
(S []) `overlaps` _ = False
_ `overlaps` (S []) = False
(S (s1:s1s)) `overlaps` (S (s2:s2s)) = rowsOverlap s1 s2
    || (S s1s) `overlaps` (S s2s)

-- | Combine two shapes in the same way that zipWith combines two lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith f  (S s1) (S s2) = S $ zipWith (\x y -> zipWith f x y) s1 s2

cmb :: Square -> Square -> Square
cmb s Nothing = s
cmb Nothing s = s
cmb (Just c1) (Just c2) = Just Black

-- | Combine two shapes.
combineShapes :: Shape -> Shape -> Shape
combineShapes s1 s2 = zipShapeWith cmb (padShapeTo (shapeSize s2) s1) (padShapeTo (shapeSize s1) s2)

