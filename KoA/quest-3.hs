import Data.List
import Data.List.Split
import Quest_3_input
import qualified Data.Map as Map
import qualified Data.Set as Set

type Graph a = Map.Map a [a]

enumerate = zip (enumFrom 0)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

neighbourFilter :: Int -> Int -> (Int, Int) -> Bool
neighbourFilter mi mj (i, j) = (i >= 0 && i < mi) && (j >= 0 && j < mj)

getNeighbours4 :: Int -> Int -> (Int, Int) -> [(Int, Int)]
getNeighbours4 mi mj (i, j) = filter (neighbourFilter mi mj) [(i-1, j), (i+1, j), (i, j-1), (i, j+1)] 

getNeighbours8 :: Int -> Int -> (Int, Int) -> [(Int, Int)]
getNeighbours8 mi mj (i, j) = [(x, y) | x <- [i-1 .. i+1], y <- [j-1 .. j+1], neighbourFilter mi mj (x, y), x /= i || y /= j] 

bfs :: Ord a => Graph a -> [a] -> Map.Map a Int
bfs g starts = bfs' g [(x, 0) | x <- starts] Map.empty
  where
    bfs' :: Ord a => Graph a -> [(a, Int)] -> Map.Map a Int -> Map.Map a Int
    bfs' g [] acc    = acc
    bfs' g ((v, level) : q) acc = bfs' (Map.delete v g) (q ++ (map (\x -> (x, level+1)) . Map.findWithDefault [] v $ g)) (Map.insertWith min v level acc)

part1 s = foldr ((+) . snd) 0 . Map.toList . bfs mappedGraph $ emptySpaces
  where
    arrayStrings = splitOn "|" s
    mi = length arrayStrings
    mj = length . head $ arrayStrings

    indexedMatrix = concat . map (\(i, arr) -> map (\(j, val) -> ((i, j), val)) arr) . enumerate . map enumerate $ arrayStrings

    mappedGraph = Map.fromList . map (\(index, val) -> (index, getNeighbours4 mi mj index)) $ indexedMatrix

    emptySpaces = map fst . filter (\(pos, v) -> v == '.') $ indexedMatrix

part3 s = foldr ((+) . snd) 0 . Map.toList . bfs mappedGraph $ emptySpaces
-- part3 s = bfs mappedGraph $ emptySpaces
-- part3 s = arrayStrings
  where
    rawArrayStrings = splitOn "|" s
    mi_ = length rawArrayStrings
    mj_ = length . head $ rawArrayStrings

    arrayStrings = [(replicate (mj_ + 2) '.')] ++ (map (\row -> "." ++ row ++ ".") rawArrayStrings) ++ [(replicate (mj_ + 2) '.')]
    mi = length arrayStrings
    mj = length . head $ arrayStrings

    indexedMatrix = concat . map (\(i, arr) -> map (\(j, val) -> ((i, j), val)) arr) . enumerate . map enumerate $ arrayStrings

    mappedGraph = Map.fromList . map (\(index, val) -> (index, getNeighbours8 mi mj index)) $ indexedMatrix

    emptySpaces = map fst . filter (\(pos, v) -> v == '.') $ indexedMatrix