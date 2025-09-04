import Quest_2_input
import Data.List
import Data.List.Split
import qualified Data.Set as Set

type SuffixArray = (String, [Int])

suffixArray :: String -> SuffixArray
suffixArray xs = (xs, sortOn (\x -> drop x xs) [0 .. (length xs)])

isPrefix :: String -> String -> Bool
isPrefix a b = take (length a) b == a

upperBound :: String -> SuffixArray -> Int
upperBound x (str, xs)   = bsearch 0 (length xs - 1)
  where
    bsearch :: Int -> Int -> Int
    bsearch l u
      | u < l            = l
      | x >= elem || (isPrefix x elem) = bsearch (mid + 1) u
      | otherwise        = bsearch l (mid - 1)
      where
        mid  = l + (u - l) `div` 2
        elem = drop (xs !! mid) str

lowerBound :: String -> SuffixArray -> Int
lowerBound x (str, xs)   = bsearch 0 (length xs - 1)
  where
    bsearch :: Int -> Int -> Int
    bsearch l u
      | u < l            = l
      | x <= elem || (isPrefix x elem) = bsearch l (mid - 1)
      | otherwise        = bsearch (mid + 1) u
      where
        mid = l + (u - l) `div` 2
        elem = drop (xs !! mid) str

countWords :: SuffixArray -> String -> Int
countWords sufA k = (upperBound k sufA) - (lowerBound k sufA)

part1 :: String -> String -> Int
part1 text words = sum . map (countWords sufA) . splitOn "," $ words
  where
    sufA = suffixArray text

genTexts :: String -> [SuffixArray]
genTexts = map suffixArray . splitOn "|"

genWords :: String -> [String]
genWords words_input = (splitOn "," $ words_input) ++ (splitOn "," . reverse $ words_input)

positionsFromWord :: SuffixArray -> String -> [Int]
positionsFromWord sufA@(str, arr) k = foldr (\x acc -> [(arr !! x) .. (arr !! x) + (length k - 1)] ++ acc) [] [(lowerBound k sufA) .. (upperBound k sufA) - 1]

-- part2 :: String -> String -> Int
-- part2 texts_input words_input = sum . map (uncurry countRunicsInText) $ [(y, x) | x <- Set.toList words, y <- texts]
part2 :: String -> String -> Int
part2 texts_input words_input = sum . map (flip countRunicsInText $ (genWords words_input)) $ (genTexts texts_input)
  where
    countRunicsInText sufA@(str, arr) words = length . Set.fromList . concat . map (positionsFromWord sufA) $ words