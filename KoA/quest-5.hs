import Data.List
import Data.List.Split
import Quest_5_input
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

type State = ([Int], [[Int]])

step :: State -> Int -> State
step (heads, state) round = ([newHead] ++ heads, newState)
  where
    clapperCol = head state
    clapper = head clapperCol
    clapperOldCol = tail clapperCol
    clapperNextCol = state !! 1
    nextColLen = length clapperNextCol
    -- 0 == left
    -- 1 == right
    clapperSide = ((clapper - 1) `div` nextColLen) `mod` 2
    clapperOffset = ((clapper - 1) `mod` nextColLen)
    clapperNewIndex = if clapperSide == 0 then clapperOffset else nextColLen - clapperOffset

    clapperNewCol = (take clapperNewIndex clapperNextCol) ++ [clapper] ++ (drop clapperNewIndex clapperNextCol)

    newState = [clapperNewCol] ++ (drop 2 state) ++ [clapperOldCol]

    correctedHead l h = (drop (4 - l `mod` 4) h) ++ (take (4 - l `mod` 4) h)
    -- newHead = (round, map head newState)
    newHeadIntList = correctedHead round (map head newState)
    newHead = read . concat . intersperse "" . map show $ newHeadIntList :: Int

steps cols k = foldl step ([], transpose cols) [1..k]

part1 dancers = head . fst . steps dancers $ 10

part2 dancers = aux (step ([], transpose dancers) 1) 1 HashMap.empty
  where
    aux s@(heads, _) round hs 
      | (HashMap.findWithDefault 0 (head heads) nhs) == 2024 = round * (head heads)
      | otherwise = aux (step s (round + 1)) (round + 1) nhs
      where
        nhs = HashMap.insertWith (+) (head heads) 1 hs

part3 dancers = aux (step ([], transpose dancers) 1) 1 HashSet.empty 0
  where
    aux s@(heads, dancers) round hs mx
      | HashSet.member dancers hs = mx
      | otherwise = aux (step s (round + 1)) (round + 1) nhs (max mx (head heads))
      where
        nhs = HashSet.insert dancers hs

main :: IO ()
main = do
    print (part1 input1)
    mapM_ print heads
    where
      (heads, endStates) = steps example1 10


  
