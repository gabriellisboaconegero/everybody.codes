import Data.List
import Data.Char
import Data.List.Split
import Quest_10_input
import qualified Data.Set as Set

enumerate = zip (enumFrom 1)

cartesianProductWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianProductWith f xs ys = [f x y | x <- xs, y <- ys]

fixInput input = (map Set.fromList (take 4 . drop 2 $ input), zipWith4 f a b c d)
  where
    a = (input !! 0)
    b = (input !! 1)
    c = (input !! 6)
    d = (input !! 7)
    f a b c d = Set.fromList [a, b, c, d]

-- part1 input = concat . map Set.toList . (uncurry $ cartesianProductWith Set.intersection) . fixInput $ input
part1 input = map Set.toList . (uncurry $ cartesianProductWith Set.intersection) . fixInput $ input

part2Aux input = sum . map (uncurry (*)) . enumerate . map (\x -> ord x - ord 'A' + 1) $ runicWord
  where
    runicWord = concat . map Set.toList . (uncurry $ cartesianProductWith Set.intersection) . fixInput $ input

part2 input = sum . map part2Aux . concat $ input
