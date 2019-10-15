{--
Module      : ConsoleGUI
Description : Runs the game on terminal/console window with keyboard input

Author      : Aatena Hasan, Theresa Van, Annie Wu
Class       : CS 4080 - Concepts of Programming Languages
Date        : 05/02/2019
--}


-- | Run a game on a text console window with keyboard input and timer ticks
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LANGUAGE BlockArguments #-}
module ConsoleGUI(module GameInterface,runGame,runGameASCII,runGame') where
import Data.List(sort)
import System.Random(randomRs,newStdGen)
import Control.Exception(bracket)
import System.Directory(getAppUserDataDirectory)
import System.IO.Error(catchIOError)
import Control.Monad(forever,when)
import Control.Concurrent(newChan,writeChan,readChan,forkIO, killThread,threadDelay,yield)
import Data.IORef
import System.IO
import System.Console.ANSI
import GameInterface
import Shapes(rows)

-- | Run a game in the terminal with colors.
-- The terminal is switched to unbuffered (raw) mode,
-- echoing is turned off,
-- the screen is cleared,
-- and the cursor is made invisible.
runGame = runGame' render 
    where
        render (game,info) =
            mapM_ renderRow (zip (rows game) (info++[""]++keys++repeat ""))

        renderRow (w,i) = mapM_ renderBlock w >> putStrLn (' ':i)

        renderBlock Nothing = putStr "  "
        renderBlock (Just c) = do setBgColor (fromEnum c)
                                  putStr "  "
                                  setNormal

-- | Run a game in the terminal with a simple ASCII character rendering, otherwise like 'runGame'
runGameASCII = runGame' render
    where
        render (game,info) =
              putStr (unlines (zipWith join ls (info++"":keys++repeat "")))
            where
                ls = lines (show game)
                join x y = x++" "++y

-- | runGame with more control over how the output from drawGame is rendered
runGame' render g =
    do aChan <- newChan
       let book0 = ScoreBook { score=0, rowCount=0 }
       start_state <- (startGame g . randomRs (0,1)) `fmap` newStdGen
       v <- newIORef (start_state,book0)
       bracket (setup aChan v) cleanup $ \ _ ->
          do homeCursor
             clearScreen
             hi <- readHiscores
             (final_state,book) <- play v aChan False hi start_state book0
             hi' <- updateHiscores hi (score book)
             renderGame final_state book hi'
             putStrLn "Game over!"
     --      return final_state
    where
        setup aChan v =
          do b <- hGetBuffering stdin
             hSetEcho stdin False
             hSetBuffering stdin NoBuffering
             t1 <- forkIO $ readActions aChan
             t2 <- forkIO $ forever $ do (t,b) <- readIORef v
                                         threadDelay (1000*tickDelay g t b)
                                         writeChan aChan (Right Tick)
             hideCursor
             return (b,t1,t2)

        cleanup (b,t1,t2) =
          do showCursor
             killThread t1
             killThread t2
             hSetBuffering stdin b
             hSetEcho stdin True

        renderGame state book hi =
          do homeCursor
             render (drawGame g state,gameInfo g state book++showHiscores hi)

        play v aChan paused hi state book =
          do writeIORef v (state,book)
             renderGame state book hi
             action <- readChan aChan
             case action of
               Right a | not paused ->
                   maybe (return (state,book)) continue (updateGame g a state)
                 where
                   continue (rows,state) =
                     play v aChan paused hi state (updateScoreBook rows book)
               Left Pause -> play v aChan (not paused) hi state book
               Left Quit  -> return (state,book)
               _ -> play v aChan paused hi state book

readActions aChan =
     do a <- readAction
        writeChan aChan a
        when (a/=Left Quit) $ readActions aChan

readAction =
     do c <- getCh
        case c of
            '\n'-> return (Right MoveDown)
            'k' -> return (Right MoveDown)
            'j' -> return (Right MoveLeft)
            'l' -> return (Right MoveRight)
            ' ' -> return (Right Rotate)
            'p' -> return (Left Pause)
            'q' -> return (Left Quit)
            _   -> readAction

keys = ["J = Left",
        "K = Down",
        "L = Right",
        "sp = Rotate", -- space bar
        "P = Pause",
        "Q = Quit"]

data OtherAction = Pause | Quit deriving Eq

-- * Keep Track of Hi Scores

updateHiscores hs s = do writeHiscores hs'; return hs'
  where hs' = take 10 . reverse . sort $ s:hs

showHiscores hs =
    "===TOP 5=== ":[pad 2 i++": "++pad 7 s|(i,s)<-zip [1..5] (hs++repeat 0)]
    where
        pad w x = replicate (w-n) ' ' ++ s
            where
                s = show x
                n = length s

hiscorePath = getAppUserDataDirectory "HsTetris"

readHiscores :: IO [Int]
readHiscores = (readIO =<< readFile =<< hiscorePath)
               `catchIOError` const (return [])

writeHiscores :: [Int] -> IO ()
writeHiscores hs = flip writeFile (show hs) =<< hiscorePath


-- * Cursor/Color Control

homeCursor  = setCursorPosition 0 0

setBgColor n = setSGR [SetColor Background vivid (toEnum (n `mod` 8))]
  where vivid = if n<8 then Vivid else Dull

setNormal  = setSGR [Reset]

-- * Unbuffered console keyboard input for Win32

getCh :: IO Char
#ifdef mingw32_HOST_OS
-- A quick hack to get unbuffered keyboard input on Windows
foreign import ccall "conio.h _getch" getch :: IO Int
getCh = do i <- getch
           if i `elem` [0,224]
             then do getch; yield; getCh -- skip function/arrow keys
             else return (toEnum i) 
#else
getCh = getChar
#endif
