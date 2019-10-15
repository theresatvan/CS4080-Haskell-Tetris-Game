{--
Module      : GameInterface
Description : Interface for the Tetris game implementation

Author      : Aatena Hasan, Theresa Van, Annie Wu
Class       : CS 4080 - Concepts of Programming Languages
Date        : 05/02/2019
--}

module GameInterface where
import Shapes(Shape)

-- | ScoreBookkeeping for Scores
data ScoreBook = ScoreBook { score,rowCount::Int }

updateScoreBook :: Int -> ScoreBook -> ScoreBook
updateScoreBook rows (ScoreBook score rowCount) = ScoreBook score' (rowCount + rows)
    where
        score' = score + 100*rows*rows

-- * User interface

-- | Game Implementation Interface
data Game state =
     Game { startGame    :: [Double] -> state,
                         -- ^ Initial state
            updateGame   :: Action -> state -> Maybe (Int,state),
                         -- ^ @Nothing@ ends the game, @Just@ continues.
                         -- The @Int@ is the number of rows that were cleared
            drawGame     :: state -> Shape,
                         -- ^ Visualize the game state
            gameInfo     :: state -> ScoreBook -> [String],
                         -- ^ Extra info to show to the user
            tickDelay    :: state -> ScoreBook -> Int,
                         -- ^ Delay between @Tick@ steps
            gameInvariant :: state -> Bool
                          -- ^ for run-time verification
          }

-- | All the actions that advances the game (user input and timer ticks)
data Action = Tick | MoveLeft | MoveRight | MoveDown | Rotate | Slam
              deriving (Eq,Read,Show,Bounded,Enum)

-- | Extra info that the user interface may display
defaultGameInfo invariant t b =
    [show (score b) ++ " points",
     show (rowCount b) ++ " rows",
     --if rowCount t>0 then show (score t `div` rowCount t)++" ppr" else "",
     if invariant t then "                          "
                    else "TETRIS INVARIANT VIOLATION"]


-- | The delay between ticks (in milliseconds) as a function of current game state
defaultDelay :: state -> ScoreBook -> Int
defaultDelay t b = round (500*0.8^(rowCount b `div` 10)) `max` 100
