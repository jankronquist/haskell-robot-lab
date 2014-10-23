-- HASKELL ROBOT LAB
--
-- This is an introductory Haskell lab. The primary objective is to implement
-- the `runAll` function that moves the robot given a string of commands. The
-- commands are `G`, `R` and `L` for Go, Right and Left respectively.
--
-- This file defines a simple main function that takes commands as input and
-- prints the resulting robot and level. Start by implementing `runAll` a couple
-- of lines down from here. After that, you can try the extras as well if you
-- want to.




-- Some imports we need, nothing to worry about.

import Control.Monad
import Data.Maybe
import Data.Either

-- Some predefined types for you to work with. If you change these some of the
-- IO stuff will have to be changed as well, so avoid that, at least to begin
-- with.

data Command = L | R | G deriving (Show)
type Robot = (Int, Int, Int) -- x, y, angle
type Level = (Int, Int) -- w, h


-- HERE WE GO!
--
-- Write your implementation below. The `runAll` function is called by the main
-- function. To start with, keep the type signature for `runAll` as it is and
-- try to implement it.

constrainRobot :: (Robot, Level, Robot) -> (Robot, Level)
constrainRobot (_, level@(width, height), robot@(x, y, _)) | x >= 0 && x <= width && y >= 0 && y <= height = (robot, level)
constrainRobot (from, level, _) = (from, level)

runStep :: Robot -> Command -> Robot
runStep (x, y, a) L = (x, y, (a + 90) `mod` 360)
runStep (x, y, a) R = (x, y, (a + 270) `mod` 360)
runStep (x, y, 0) G = (x+1, y, 0)
runStep (x, y, 90) G = (x, y+1, 90)
runStep (x, y, 180) G = (x-1, y, 180)
runStep (x, y, 270) G = (x, y-1, 270)
runStep state _ = state

constrainedStep :: (Robot, Level) -> Command -> (Robot, Level)
constrainedStep (robot, level) command = constrainRobot (robot, level, runStep robot command)


runAll :: Robot -> Level -> [Command] -> Robot
runAll robot level cmds = fst $ foldl constrainedStep (robot, level) cmds


parse :: Char -> Maybe Command
parse 'L' = Just L
parse 'R' = Just R
parse 'G' = Just G
parse _ = Nothing

parseAll :: String -> [Command]
parseAll string = catMaybes $ map parse string


parse2 :: Char -> Either Char Command
parse2 'L' = Right L
parse2 'R' = Right R
parse2 'G' = Right G
parse2 c = Left c

leftsOrRights :: [Either a b] -> Either [a] [b]
leftsOrRights stuff = let l = lefts stuff
            in if null l then (Right $ rights stuff) else Left l
            
parseAll2 :: String -> Either String [Command]
parseAll2 s = leftsOrRights $ map parse2 s

-- When you have a working `runAll` you can move on by...
--
-- * restraining the robot from moving outside the level.
-- * validating the incoming commands and print an error message if invalid.
-- * converting the Command type to an algebraic data type.
--
-- Note that some of these extras might require you to change the main function.





--  The rest of this file is only IO stuff, you can ignore it.

tileChar :: (Int, Int) -> Robot -> String
tileChar (x, y) (rx, ry, _)
  | x == rx && y == ry  = "O"
  | otherwise           = "."

printLevel :: Level -> Robot -> String
printLevel (w, h) r =
  unlines $ map unwords l
  where l = do x <- [0..w]
               return $ do
                   y <- [0..h]
                   return $ tileChar (x, y) r

level :: Level
level = (20, 20)

startRobot :: Robot
startRobot = (10, 10, 0)

main :: IO ()
main = next startRobot
  where next r = do
          cmds <- getLine
          unless (null cmds) $ do
            let cs = parseAll cmds
            let r2 = runAll r level cs
            putStr $ printLevel level r2
            next r2
