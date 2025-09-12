{-# LANGUAGE ScopedTypeVariables #-}
import Data.List
import Data.Char
import Data.Maybe
import Data.List.Split
import Quest_14_input
import qualified Data.Map as Map
import qualified Data.Set as Set

sums = scanl (+) 0
sums1 = scanl1 (+)

part1 input = maximum . sums $ [f dir * v | (dir, v) <- input, dir `elem` ['U', 'D']]
  where
    f :: Char -> Int
    f 'U' = 1
    f 'D' = -1
    f 'R' = 1
    f 'L' = -1
    f 'F' = 1
    f 'B' = -1
    f _   = 0

g (l, (x, y, z)) ('U', v) = (l++[(x, y + i, z) | i <- [1..v]], (x, y + v, z))
g (l, (x, y, z)) ('D', v) = (l++[(x, y - i, z) | i <- [1..v]], (x, y - v, z))
g (l, (x, y, z)) ('R', v) = (l++[(x + i, y, z) | i <- [1..v]], (x + v, y, z))
g (l, (x, y, z)) ('L', v) = (l++[(x - i, y, z) | i <- [1..v]], (x - v, y, z))
g (l, (x, y, z)) ('F', v) = (l++[(x, y, z + i) | i <- [1..v]], (x, y, z + v))
g (l, (x, y, z)) ('B', v) = (l++[(x, y, z - i) | i <- [1..v]], (x, y, z - v))

part2 input = length . Set.unions . map (Set.fromList . fst . foldl g ([], (0, 0, 0))) $ input

type Graph a         = Map.Map a [a]
type IndexedMatrix a = (Int, Int, [((Int, Int), a)])
type MatNeighbours   = Int -> Int -> (Int, Int) -> [(Int, Int)]
type CostFunction a  = a -> a -> Int

enumerate = zip (enumFrom 0)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

neighbourFilter :: Int -> Int -> (Int, Int) -> Bool
neighbourFilter mi mj (i, j) = (i >= 0 && i < mi) && (j >= 0 && j < mj)

getNeighbours4 :: MatNeighbours
getNeighbours4 mi mj (i, j) = [(i, j) | (i, j) <- [(i-1, j), (i+1, j), (i, j-1), (i, j+1)], neighbourFilter mi mj (i, j)]

getNeighbours8 :: MatNeighbours
getNeighbours8 mi mj (i, j) = [(x, y) | x <- [i-1 .. i+1], y <- [j-1 .. j+1], neighbourFilter mi mj (x, y), x /= i || y /= j] 

getNeighbours6 :: (Int, Int, Int) -> [(Int, Int, Int)]
getNeighbours6 (i, j, k) = [
  (i-1, j, k),
  (i+1, j, k),
  (i, j-1, k),
  (i, j+1, k),
  (i, j, k-1),
  (i, j, k+1)]

-- Given a graph and a list of start nodes, find distance to all others nodes
-- from one of the start points. path recoverable with parent tracking
dijkstra :: forall a. Ord a => CostFunction a -> [a] -> Graph a -> Map.Map a (a, Int)
dijkstra cost starts g = dij' g (Set.fromList [(0, (x, x)) | x <- starts]) Map.empty
  where
    -- given a queue, append the neighbours of the next node
    queueAppendNeighbours :: Graph a -> Set.Set (Int, (a, a)) -> (Int, (a, a)) -> Set.Set (Int, (a, a))
    queueAppendNeighbours g q (level, (_, v)) = q `Set.union` (Set.fromList . map (\x -> (level + cost v x, (v, x))) . Map.findWithDefault [] v $ g)

    -- given a pair of nodes return the one with lowest level
    lowestNode :: (a, Int) -> (a, Int) -> (a, Int)
    lowestNode v1@(_, l1) v2@(_, l2) 
      | l2 < l1  = v2
      |otherwise = v1

    dij' :: Graph a -> Set.Set (Int, (a, a)) -> Map.Map a (a, Int) -> Map.Map a (a, Int)
    dij' g s acc
      | length s == 0 = acc
      | otherwise =
        let
          (v@(level, (parent, label)), q) = Set.deleteFindMin s
        in dij' (Map.delete label g) (queueAppendNeighbours g q v) (Map.insertWith lowestNode label (parent, level) acc)

part3 input = (result, minimum result)
  where
    result = [Map.foldr (\(_, l) acc -> acc+l) 0 . Map.filterWithKey (\k a -> Set.member k leafs) . dijkstra (const . const $ 1) [t] $ graph | t <- mainTrunk]
    mainTrunk = [(0, y, 0) | y <- [1..tallest]]
    graph = Map.fromList . map (\x -> (x, getNeighbours6 x)) . Set.toList $ tree
    tallest = fromMaybe tallest' $ do
      v <- Set.lookupMin (Set.fromList [1..tallest'] `Set.difference` Set.map (\(_, y, _) -> y) mainTrunk')
      return (v-1)

    (_, tallest', _) = Set.findMax mainTrunk'
    mainTrunk' = Set.filter (\(x, y, z) -> x == 0 && z == 0) tree
    tree = Set.unions . map (Set.fromList . fst) $ treeParts
    leafs =  Set.fromList . map snd $ treeParts
    treeParts = map (foldl g ([], (0, 0, 0))) input
