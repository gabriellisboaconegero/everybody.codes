import Data.List
import Data.Char
import Data.List.Split
import Quest_11_input
import qualified Data.Map as Map

cartesianProductWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianProductWith f xs ys = [f x y | x <- xs, y <- ys]

nextForLetter m l t = Map.foldrWithKey (\k v acc -> acc + (Map.findWithDefault 0 k ll) * v) 0 t
  where
    ll = Map.findWithDefault Map.empty l m

f (a, _) (b, bs) = Map.singleton a $ Map.singleton b (length . filter (== a) $ bs)

execute 1 f a = a
execute n f a = execute (n-1) f (f a)

part1 input = map (sum . Map.elems) . take 4 . iterate step $ (step t)
  where
    m = Map.unionsWith (Map.union) . cartesianProductWith f input $ input
    t = Map.mapWithKey (\k _ -> if k == 'A' then 1 else 0) m
    step t = Map.mapWithKey (\k v -> nextForLetter m k t) t

part2 input = map (Map.foldrWithKey (\_ v acc -> acc + v) 0). take 10 . iterate step $ (step t)
  where
    m = Map.unionsWith (Map.union) . cartesianProductWith f input $ input
    t = Map.mapWithKey (\k _ -> if k == 'Z' then 1 else 0) m
    step t = Map.mapWithKey (\k v -> nextForLetter m k t) t

part3 input = (\xs -> maximum xs - minimum xs) . map (gen 20) $ Map.keys m
  where
    m = Map.unionsWith (Map.union) . cartesianProductWith f input $ input
    t l = Map.mapWithKey (\k _ -> if k == l then 1 else 0) m
    step t = Map.mapWithKey (\k v -> nextForLetter m k t) t
    gen n l = sum . Map.elems . execute n step $ (step $ t l)

  