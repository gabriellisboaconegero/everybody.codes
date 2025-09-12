import Data.List
import Data.Char
import Data.List.Split
import Quest_12_input
import qualified Data.Map as Map
import qualified Data.Set as Set

cartesianProductWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianProductWith f xs ys = [f x y | x <- xs, y <- ys]

enumerate = zip [0..]

rank 'A' = 1
rank 'B' = 2
rank 'C' = 3
rank 'T' = 1
rank 'H' = 2
rank _   = 0

d x0 x1 = abs (x0 - x1)

catapults enumInput =
  let
    f i j v = if v `elem` ['A', 'B', 'C'] then Map.singleton v (j, i) else Map.empty
  in foldr ( \(i, xs) m -> Map.union (foldr ( \(j, v) m -> Map.union (f i j v) m) m xs) m) Map.empty enumInput

targets enumInput =
  let
    f i j v = if v `elem` ['T', 'H'] then Set.singleton (v, (j, i)) else Set.empty
  in foldr ( \(i, xs) m -> Set.union (foldr ( \(j, v) m -> Set.union (f i j v) m) m xs) m) Set.empty enumInput

enumInput = enumerate . map enumerate . reverse

part1 input = sum . map (\(l, v, _, _) -> rank l * (v `div` 3)) . filter (\(_, v, _, _) -> v `mod` 3 == 0) $ shots
  where
    shots = cartesianProductWith (\(v, (x0, y0)) (_, (x1, y1)) -> (v, d x0 x1 + y1 - y0, x1, y1)) (Map.toList c) (Set.toList t)
    t = targets i'
    c = catapults i'
    i' = enumInput input

part2 input = sum scores 
  where
    scores = map (\(v, (x, y)) -> ((x+y) `div` 3) * ((x + y) `mod` 3 + 1) * (rank v)) (Set.toList t)
    t = targets i'
    i' = enumInput input

part3 input = sum i1' + sum i2' + sum i3'
  where
    f1 (x, y) = ((x+y) `div` 3) * ((x + y) `mod` 3 + 1)
    f2 (x, y) = y
    f3 (x, y) = min x y * (abs (x - y) + 1)

    t1 (x, y) = 0 <= y && y <= fl x
    t2 (x, y) = fl x < y && y <= x
    t3 (x, y) = x < y && y <= x + 2

    i1 = filter t1 i'
    i2 = filter t2 i'
    i3 = filter t3 i'

    i1' = map f1 i1
    i2' = map f2 i2
    i3' = map f3 i3

    i' = map (\(x, y) -> (fl x, y - cl x)) input
    fl x = x `div` 2
    cl x = fl x + x `mod` 2