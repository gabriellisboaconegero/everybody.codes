{-# LANGUAGE ScopedTypeVariables #-}
import Data.Char
import Data.Maybe
import Data.List
import Quest_19_input
import qualified Data.Map as Map
import qualified Data.Set as Set

type IndexedMatrix a = (Int, Int, [((Int, Int), a)])

enumerate = zip (enumFrom 0)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

indexedMatrix :: [[a]] -> IndexedMatrix a
indexedMatrix input = (mi, mj, concat . map (\(i, arr) -> map (\(j, val) -> ((i, j), val)) arr) . enumerate . map enumerate $ input)
  where
    mi = length $ input
    mj = length . head $ input

dist :: (Int, Int) -> (Int, Int) -> Int
dist (x1, y1) (x2, y2) = abs (x2-x1 + y2-y1) 

getNeighbours8 :: (Int, Int) -> [(Int, Int)]
getNeighbours8 (i, j) = [(x, y) | x <- [i-1 .. i+1], y <- [j-1 .. j+1], x /= i || y /= j] 

rotateR :: (Int, Int) -> (Int, Int) -> (Int, Int)
rotateR rot@(i1, j1) p@(i2, j2)
  | p == (i1 - 1, j1 - 1) = (i1 - 1, j1 + 0)
  | p == (i1 - 1, j1 + 0) = (i1 - 1, j1 + 1)
  | p == (i1 - 1, j1 + 1) = (i1 + 0, j1 + 1)
  | p == (i1 + 0, j1 - 1) = (i1 - 1, j1 - 1)
  | p == (i1 + 0, j1 + 1) = (i1 + 1, j1 + 1)
  | p == (i1 + 1, j1 - 1) = (i1 + 0, j1 - 1)
  | p == (i1 + 1, j1 + 0) = (i1 + 1, j1 - 1)
  | p == (i1 + 1, j1 + 1) = (i1 + 1, j1 + 0)
  | otherwise = p

rotateL :: (Int, Int) -> (Int, Int) -> (Int, Int)
rotateL rot@(i1, j1) p@(i2, j2)
  | p == (i1 - 1, j1 - 1) = (i1 + 0, j1 - 1)
  | p == (i1 - 1, j1 + 0) = (i1 - 1, j1 - 1)
  | p == (i1 - 1, j1 + 1) = (i1 - 1, j1 + 0)
  | p == (i1 + 0, j1 - 1) = (i1 + 1, j1 - 1)
  | p == (i1 + 0, j1 + 1) = (i1 - 1, j1 + 1)
  | p == (i1 + 1, j1 - 1) = (i1 + 1, j1 + 0)
  | p == (i1 + 1, j1 + 0) = (i1 + 1, j1 + 1)
  | p == (i1 + 1, j1 + 1) = (i1 + 0, j1 + 1)
  | otherwise = p

rotate :: Char -> (Int, Int) -> (Int, Int) -> (Int, Int)
rotate 'R' = rotateR 
rotate 'L' = rotateL

part1 (rotations, lines) = do
  ((i1, j1), _) <- Map.lookup end0 endMap
  ((i2, j2), _) <- Map.lookup end1 endMap
  return $ map (\x -> Map.findWithDefault '?' (i1, x) coEndMap) [j1+1..j2-1]
  where
    coEndMap = Map.fromList $ Map.elems endMap
    endMap = foldl (\acc (p, r) -> Map.map (\(x, v) -> (rotate r p x, v)) acc) (Map.fromList $ map (\x -> (x, x)) mat) $ zip rotPoints $ (concat $ repeat rotations)
    (mi, mj, mat) = indexedMatrix lines
    rotPoints = map fst $ filter (\((i, j),_) -> not (i `elem` [0, mi-1]) && not (j `elem` [0, mj-1])) mat
    end0 = fromMaybe ((0,0), '.') $ find (\(_,c) -> c == '>') mat
    end1 = fromMaybe ((0,0), '.') $ find (\(_,c) -> c == '<') mat

part2 (rotations, lines) = do
  ((i1, j1), _) <- find ((== '>') . snd) endMap
  ((i2, j2), _) <- find ((== '<') . snd) endMap
  return $ map (\x -> Map.findWithDefault '?' (i1, x) coEndMap) [j1+1..j2-1]
  where
    aux n grid = foldr (\_ acc -> round acc) grid [1..n]

    endMap = aux 100 (filter ((/= '.') . snd) mat)
    coEndMap = Map.fromList endMap

    round grid = map (\(p, v) -> (Map.findWithDefault (0,0) p transMap, v)) grid

    transMap = foldl (\acc (p, r) -> Map.map (rotate r p) acc) (Map.fromList $ map (\x -> (x, x)) mat') $ zip rotPoints $ (concat $ repeat rotations)
    (mi, mj, mat) = indexedMatrix lines
    mat' = map fst mat
    rotPoints = map fst $ filter (\((i, j),_) -> not (i `elem` [0, mi-1]) && not (j `elem` [0, mj-1])) mat
    end0 = fromMaybe ((0,0), '.') $ find (\(_,c) -> c == '>') mat
    end1 = fromMaybe ((0,0), '.') $ find (\(_,c) -> c == '<') mat

part3 (rotations, lines) = do
  ((i1, j1), _) <- find ((== '>') . snd) endMap
  ((i2, j2), _) <- find ((== '<') . snd) endMap
  return $ map snd $ sort $ filter (\((i, j), _) -> i == i1 && (j > j1 && j < j2)) endMap
  where
    coEndMap = Map.fromList endMap
    endMap = aux 1048576000 (filter ((/= '.') . snd) mat)
    aux n grid = map (\(p, c) -> (getPosOnRound n p, c)) grid
    getPosOnRound n p =
      let
        c = Map.findWithDefault [] p cycles
      in c !! (n `mod` length c)

    cycles = Map.map (\x -> findCycleStartingAt x x []) transMap0
    findCycleStartingAt src endPoint acc
      | endPoint' == src = endPoint:acc
      | otherwise = endPoint:(findCycleStartingAt src endPoint' acc)
      where
        endPoint' = (Map.findWithDefault (0,0) endPoint transMap)

    transMap = Map.fromList $ map (uncurry $ flip (,)) $ Map.toList $ foldl (\acc (p, r) -> updateTrans acc $ updateNeighbours p r) transMap0 rotationSeq

    updateTrans trans updates =
      let
        get u = Map.findWithDefault (0,0) u trans
        f (u, v) acc = Map.insert v (get u) acc
      in foldr f trans updates

    transMap0 = Map.fromList $ map (\x -> (x, x)) mat'
    rotationSeq = zip rotPoints $ (concat $ repeat rotations)
    updateNeighbours p r = map (\x -> (x, rotate r p x)) $ getNeighbours8 p

    (mi, mj, mat) = indexedMatrix lines
    mat' = map fst mat
    rotPoints = map fst $ filter (\((i, j),_) -> not (i `elem` [0, mi-1]) && not (j `elem` [0, mj-1])) mat

    end0 = fromMaybe ((0,0), '.') $ find (\(_,c) -> c == '>') mat
    end1 = fromMaybe ((0,0), '.') $ find (\(_,c) -> c == '<') mat