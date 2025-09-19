{-# LANGUAGE ScopedTypeVariables #-}
import Data.List
import Data.Char
import Data.Maybe
import Quest_1_input
import qualified Data.Map as Map

sums = scanl (+) 0

-- bsearch :: Integral a => (a -> a) -> a -> a
bsearch f m = bsearch' 0 m
  where
    bsearch' a b
      | b < a      = b
      | m >= f mid = bsearch' (mid + 1) b
      | otherwise  = bsearch' a (mid - 1)
      where
        mid = a + (b - a) `div` 2

square x = x * x

-- fastExpM :: Integral a => a -> a -> a -> a
-- b^x `mod` m
fastExpM b x m
  | x == 0         = 1
  | x == 1         = b
  | x `mod` 2 == 1 = mo (res * b)
  | otherwise      = res
  where
    x2 = x `div` 2
    mo a = a `mod` m
    res = mo (square (fastExpM b x2 m))

eni :: Int -> Int -> Int -> Int -> Int
eni qtd n exp m = read $ concat $ map show $ reverse $ drop 1 expList
  where
    expList = scanl (\acc x -> ((acc `mod` m) * (n `mod` m)) `mod` m) startVal [1..v]
    startVal = fastExpM n (exp-v) m
    v = min exp qtd

floydCycleFinding :: Int -> (Int -> Int) -> (Int, Int)
floydCycleFinding x0 f = (mu, lambda)
  where
    findCycle hare tortoise
      | hare == tortoise = (hare, tortoise)
      | otherwise = findCycle (f (f hare)) (f tortoise)

    findMu mu hare tortoise
      | hare == tortoise = (mu, hare, tortoise)
      | otherwise = findMu (mu+1) (f (f hare)) (f tortoise)

    findLambda lambda hare tortoise
      | hare == tortoise = (lambda, hare, tortoise)
      | otherwise = findLambda (lambda+1) (f hare) tortoise

    (hare, tortoise) = findCycle (f (f x0)) (f x0)
    (mu, hare', tortoise') = findMu 0 x0 tortoise
    (lambda, hare'', tortoise'') = findLambda 1 (f tortoise') tortoise'

cycleFinding :: Int -> (Int -> Int) -> (Int, Int)
cycleFinding x0 f = aux x0 f (Map.singleton x0 0) 0
  where
    aux x f s c
      | isJust $ Map.lookup (f x) s = (mu, lam)
      | otherwise = aux (f x) f (Map.insert (f x) (c+1) s) (c+1)
      where
        mu = fromMaybe 0 $ Map.lookup (f x) s
        lam = length s - mu

eni2 n exp m = (exp' `div` l * sum_) + (sums_ !! (exp' `mod` l)) + minSum
  where
    expList = scanl (\acc x -> f acc) startVal [1..mu+lam-1]
    (mu, lam) = cycleFinding startVal f
    l = length $ drop mu expList
    startVal = f 1
    f x = ((x `mod` m) * (n `mod` m)) `mod` m

    exp' = exp - mu
    minSum = sum $ take mu expList
    sum_ = sum $ drop mu expList
    sums_ = sums $ drop mu expList


solveSet1 (a:b:c:x:y:z:m:_) = eni x a x m + eni y b y m + eni z c z m
solveSet2 (a:b:c:x:y:z:m:_) = eni 5 a x m + eni 5 b y m + eni 5 c z m
solveSet3 (a:b:c:x:y:z:m:_) = eni2 a x m + eni2 b y m + eni2 c z m

part1 input = maximum $ map solveSet1 input
part2 input = maximum $ map solveSet2 input
part3 input = maximum $ map solveSet3 input