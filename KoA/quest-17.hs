{-# LANGUAGE ScopedTypeVariables #-}
import Data.List
import Data.Char
import Data.Maybe
import Quest_17_input
import qualified Data.Map as Map
import qualified Data.Set as Set

type Graph a         = Map.Map a [a]
type IndexedMatrix a = (Int, Int, [((Int, Int), a)])
type MatNeighbours   = Int -> Int -> (Int, Int) -> [(Int, Int)]
type CostFunction a  = a -> a -> Int
type Edge a          = (a, a, Int)

enumerate = zip (enumFrom 0)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

neighbourFilter :: Int -> Int -> (Int, Int) -> Bool
neighbourFilter mi mj (i, j) = (i >= 0 && i < mi) && (j >= 0 && j < mj)

getNeighbours4 :: MatNeighbours
getNeighbours4 mi mj (i, j) = [(i, j) | (i, j) <- [(i-1, j), (i+1, j), (i, j-1), (i, j+1)], neighbourFilter mi mj (i, j)]

getNeighbours8 :: MatNeighbours
getNeighbours8 mi mj (i, j) = [(x, y) | x <- [i-1 .. i+1], y <- [j-1 .. j+1], neighbourFilter mi mj (x, y), x /= i || y /= j] 

prim :: forall a. Ord a => CostFunction a -> Graph a -> (Map.Map a a, Int)
prim cost g = prim' g Map.empty (Set.singleton (0, head $ Map.keys g, head $ Map.keys g)) 0
  where
    queueAppendNeighbours g edges v = Set.union edges $ Set.fromList $ map (\u -> (cost u v, v, u)) $ Map.findWithDefault [] v g

    prim' g g' edges sumMin
      | length edges == 0 = (g', sumMin)
      | isJust $ Map.lookup v g' = prim' g g' edges' sumMin
      | otherwise = prim' (Map.delete v g) (Map.insert v u g') (queueAppendNeighbours g edges' v) (sumMin + c)
      where
        (e@(c, u, v), edges') = Set.deleteFindMin edges

prim2 :: forall a. Ord a => CostFunction a -> Graph a -> (Map.Map a a, Int)
prim2 cost g = prim' g Map.empty (Set.singleton (0, head $ Map.keys g, head $ Map.keys g)) 0
  where
    queueAppendNeighbours g edges v = Set.union edges $ Set.fromList $ filter (\(d, _, _) -> d < 6) $ map (\u -> (cost u v, v, u)) $ Map.findWithDefault [] v g

    prim' g g' edges sumMin
      | length edges == 0 = (g', sumMin)
      | isJust $ Map.lookup v g' = prim' g g' edges' sumMin
      | otherwise = prim' (Map.delete v g) (Map.insert v u g') (queueAppendNeighbours g edges' v) (sumMin + c)
      where
        (e@(c, u, v), edges') = Set.deleteFindMin edges


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

recoverPath :: forall a b. Ord a =>  a -> Map.Map a (a, b) -> Maybe [(a, b)]
recoverPath v g = do
  l <- Map.lookup v g
  return (recoverPath' (v, l) g)
  where
    recoverPath' :: (a, (a, b)) -> Map.Map a (a, b) -> [(a, b)]
    recoverPath' (v, (parent, x)) g
      | v == parent = [(parent, x)]
      | otherwise   = (v, x) : recoverPath' (parent, (Map.findWithDefault (parent, x) parent g)) g

indexedMatrix :: [[a]] -> IndexedMatrix a
indexedMatrix input = (mi, mj, concat . map (\(i, arr) -> map (\(j, val) -> ((i, j), val)) arr) . enumerate . map enumerate $ input)
  where
    mi = length $ input
    mj = length . head $ input

indexedMatrixToGraph :: Ord a => MatNeighbours -> IndexedMatrix a -> Graph ((Int, Int), a)
indexedMatrixToGraph neighbours (mi, mj, indexedMatrix) = Map.fromList . map (\(index, val) -> ((index, val), map (\(i, j) -> ((i, j), Map.findWithDefault val (i, j) indexedMatrixM)) . neighbours mi mj $ index)) $ indexedMatrix
  where
    indexedMatrixM = Map.fromList indexedMatrix

distModulus x y m = 
  let
    k = (y `mod` m + (m - x `mod` m)) `mod` m
  in min k (m-k)

part1 input = s + length graph
  where
    (g, s) = prim cost graph
    graph = indexedMatrixToGraph neighbours (mi, mj, stars)
    cost ((i0, j0), _) ((i1, j1), _) = abs (i0-i1) + abs (j0-j1)
    ii'@(mi, mj, mat) = indexedMatrix $ input
    neighbours mi mj v@(i, j) = map fst $ filter ((/= v) . fst) stars
    stars = filter ((== '*') . snd) mat

part3 input = product $ take 3 $ reverse $ sort $ aux [] graph
  where
    aux acc graph
      | length graph == 0 = acc
      | otherwise =
        let
          (g, s) = prim2 cost graph
        in aux ((s + length g):acc) $ Map.filterWithKey (\k _ -> isNothing $ Map.lookup k g) graph
    graph = indexedMatrixToGraph neighbours (mi, mj, stars)
    cost ((i0, j0), _) ((i1, j1), _) = abs (i0-i1) + abs (j0-j1)
    ii'@(mi, mj, mat) = indexedMatrix $ input
    neighbours mi mj v@(i, j) = map fst $ filter ((/= v) . fst) stars
    stars = filter ((== '*') . snd) mat