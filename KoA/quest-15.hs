{-# LANGUAGE ScopedTypeVariables #-}
import Data.List
import Data.Char
import Data.Maybe
import Data.List.Split
import Quest_15_input
import qualified Data.Map as Map
import qualified Data.Set as Set

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

part1 input = 2 * minimum [snd . Map.findWithDefault (start, 0) e $ dij | e <- ends]
  where
    dij = dijkstra (const . const $ 1) [start] . indexedMatrixToGraph neighbours $ ii'

    ii'@(mi, mj, mat) = indexedMatrix $ input
    neighbours mi mj v@(i, j) = filter (\(i, j) -> (/= '#') . snd $ (mat !! (i*mj+j))) . getNeighbours4 mi mj $ v
    start = head . filter (\((i, j), s) -> s == '.' && i == 0) $ mat
    ends   = filter ((== 'H') . snd) $ mat

part2 input = minimum . map f $ [dijkstra cost [start] . createG $ perm | perm <- permsOfDests]
  where
    f ma = fromMaybe 0 $ do
      (_, d) <- Map.lookup start' ma
      return d

    cost v u@(_, c)
      | c == '@'  = fromMaybe 0 $ do (_, d) <- Map.lookup start parentDists; return d
      | otherwise = fromMaybe 0 $ do (_, d) <- Map.lookup u parentDists; return d
      where
        parentDists = Map.findWithDefault Map.empty v endsDists

    createG (x:xs) = Map.union (Map.singleton start (filter ((== x) . snd) ends)) (createG' (x:xs))

    createG' [x]    = Map.fromList [(e, [start']) | e@(_, v) <- ends, v == x]
    createG' (x:y:xs) =
      let
        nei = (filter ((== y) . snd) ends)
      in Map.union (Map.fromList [(e, nei) | e@(_, v) <- ends, v == x]) (createG' (y:xs))

    permsOfDests = permutations . nub . map snd $ ends
    endsDists = Map.fromList [((,) e) . Map.filterWithKey (\v@(_, c) _ -> (c /= '.' || v == start || v == start') && c /= (snd e)) . dij $ e | e <- start:ends]

    dij start = dijkstra (const . const $ 1) [start] graph

    graph = indexedMatrixToGraph neighbours $ ii'

    ii'@(mi, mj, mat) = indexedMatrix $ input

    neighbours mi mj v@(i, j) = filter (\(i, j) -> (\x -> x /= '#' && x /= '~') . snd $ (mat !! (i*mj+j))) . getNeighbours4 mi mj $ v

    start = head . filter (\((i, j), s) -> s == '.' && i == 0) $ mat
    start' = (fst start, '@')
    ends   = filter ((flip elem $ ['A'..'Z']) . snd) $ mat