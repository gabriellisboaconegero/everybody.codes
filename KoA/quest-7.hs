import Data.List
import Data.List.Split
import Quest_7_input

inside f (x, a) = (x, f a)
sums = scanl (+) 0
sums1 = scanl1 (+)

toFunc :: Char -> Int
toFunc '+' = 1
toFunc '-' = -1
toFunc '=' = 0
toFunc _   = 0

h f r v = r * v + (sum f) * n + sizeF * x * (n * (n - 1)) `div` 2 + n * x * rm + ((sums f) !! rm)
  where
    sizeF = length f
    n = r `div` sizeF
    rm = r `mod` sizeF
    x = (head . reverse $ f)

solve total nums input = h (sums1 input) nums total

part1 input = concat . reverse . map fst . sortBy (\(_, a) (_, b) -> compare a b) . map (inside $ solve 10 10) $ map (inside $ map toFunc) input

lcm' l1 l2 = (lcm (length l1) (length l2)) `div` (length l1)
expand p t = concat . take (lcm' p t) . repeat $ p
merge t p = zipWith merge' (expand p t) (expand t p)
  where
    merge' _ '+' = '+'
    merge' _ '-' = '-'
    merge' x _   = x

part2 input tra = inputFinal
  where
    input' = map (inside $ map toFunc . merge tra) input
    inputSolved = map (inside $ solve 10 (10 * (length tra))) input'
    inputFinal = concat . reverse . map fst . sortBy (\(_, a) (_, b) -> compare a b) $ inputSolved

genPlans a b c = genPlans' (a+1) (b+1) (c+1)
  where
    genPlans' 0 _ _ = []
    genPlans' _ 0 _ = []
    genPlans' _ _ 0 = []
    genPlans' 1 1 1 = [""]
    genPlans' cPlus cMinus cEquals = genPlus ++ genMinus ++ genEquals
      where
        genPlus   = map ((:) '+') (genPlans' (cPlus - 1) cMinus cEquals)
        genMinus  = map ((:) '-') (genPlans' cPlus (cMinus - 1) cEquals)
        genEquals = map ((:) '=') (genPlans' cPlus cMinus (cEquals - 1))

part3 input tra = length plansSolved
  where
    input' = map toFunc . merge tra . snd . head $ input
    inputSolved = solve 10 (2024 * (length tra)) input'

    plans' = map (map toFunc . merge tra) $ genPlans 5 3 3
    plansSolved = filter (\x -> x > inputSolved) . map (solve 10 (2024 * (length tra))) $ plans'