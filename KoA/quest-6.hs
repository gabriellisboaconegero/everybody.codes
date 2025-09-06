import Data.List
import Data.List.Split
import Quest_6_input
import qualified Data.Map as Map

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

-- Given a graph and a list of start nodes, find distance to all others nodes
-- from one of the start points. path recoverable with parent tracking
bfs :: Ord a => Graph a -> [a] -> Map.Map a (a, Int)
bfs g starts = bfs' g [(x, (x, 0)) | x <- starts] Map.empty
  where
    bfs' :: Ord a => Graph a -> [(a, (a, Int))] -> Map.Map a (a, Int) -> Map.Map a (a, Int)
    bfs' g [] acc    = acc
    bfs' g (v@(label, u@(parent, level)) : q) acc = bfs' (Map.delete label g) (queueAppendNeighbours g q v) (Map.insertWith lowestNode label u acc)
      where
        -- given a queue, append the neighbours of the next node
        queueAppendNeighbours :: Ord a => Graph a -> [(a, (a, Int))] -> (a, (a, Int)) -> [(a, (a, Int))]
        queueAppendNeighbours g q (v, (_, level)) = q ++ (map (\x -> (x, (v, level + 1))) . Map.findWithDefault [] v $ g)
    
        -- given a pair of nodes return the one with lowest level
        lowestNode :: (a, Int) -> (a, Int) -> (a, Int)
        lowestNode v1@(_, l1) v2@(_, l2) 
          | l2 < l1  = v2
          |otherwise = v1

recoverPath :: Ord a => Map.Map a (a, b) -> (a, (a, b))-> [a]
recoverPath g (v, (parent, x))
  | v == parent = [parent]
  | otherwise   = v : recoverPath g (parent, (Map.findWithDefault (parent, x) parent g))

part1 input = concat . reverse . ((:) "@") . recoverPath nodesDistsFromRR $ res
  where
    g' = Map.fromList input
    g = foldr (\x acc -> Map.insertWith (flip const) x [] acc) g' (concat . Map.elems $ g')
    nodesDistsFromRR = bfs g ["RR"]
    nodesWithoutLeafs = map fst . filter (\(k, v) -> all (/= "@") v || length v <= 0) $ (Map.toList g)
    nodesDistsWithLeafs = foldr (\x acc -> Map.delete x acc) nodesDistsFromRR nodesWithoutLeafs
    res = head . head . filter ((== 1) . length) . groupBy (aux (==))  . sortBy (aux compare) . Map.toList $ nodesDistsWithLeafs
    -- res = groupBy (aux (==))  . sortBy (aux compare) . Map.toList $ nodesDistsWithLeafs

    -- aux :: (a -> b -> c) -> (d, (e, a)) -> (f, (g, b)) -> c
    aux f (_, (_, a)) (_, (_, b)) = f a b

part2 input = reverse . ((:) "@") . recoverPath nodesDistsFromRR $ res
  where
    g' = Map.fromList input
    g = foldr (\x acc -> Map.insertWith (flip const) x [] acc) g' (concat . Map.elems $ g')
    nodesDistsFromRR = bfs g ["RR"]
    nodesWithoutLeafs = map fst . filter (\(k, v) -> all (/= "@") v || length v <= 0) $ (Map.toList g)
    nodesDistsWithLeafs = foldr (\x acc -> Map.delete x acc) nodesDistsFromRR nodesWithoutLeafs
    res = head . head . filter ((== 1) . length) . groupBy (aux (==))  . sortBy (aux compare) . Map.toList $ nodesDistsWithLeafs
    -- res = groupBy (aux (==))  . sortBy (aux compare) . Map.toList $ nodesDistsWithLeafs

    -- aux :: (a -> b -> c) -> (d, (e, a)) -> (f, (g, b)) -> c
    aux f (_, (_, a)) (_, (_, b)) = f a b
