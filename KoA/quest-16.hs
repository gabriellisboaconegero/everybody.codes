{-# LANGUAGE ScopedTypeVariables #-}
import Data.List
import Data.Char
import Data.Maybe
import Quest_16_input
import qualified Data.Map as Map
import qualified Data.Set as Set

sums = scanl (+) 0

part1 (turns, wheels) = concat $ intersperse " " result
  where
    result = map (\(t, w) -> w !! (100 * t `mod` (length w))) $ zip turns wheels

part2 (turns, wheels) = accumulativeCoins 202420242024
  where
    wheelsTurns = zip turns wheels
    turnsPerCycle = foldr1 lcm $ map (\(t, w) -> lcm t (length w) `div` t) wheelsTurns
    getTurn n = concat $ map (\(t, w) -> w !! (n * t `mod` (length w))) wheelsTurns
    (sumsPerCycle, sumPerCycle) =
      let
        a = map (coinsOnTurn . getTurn) [1..turnsPerCycle]
      in (sums a, sum a)

    coinsOnTurn =
      let
        f xs
          | length xs >= 3 = 1 + (length xs - 3)
          | otherwise      = 0
      in sum . map f . group . sort

    accumulativeCoins turn = turn `div` turnsPerCycle * sumPerCycle + sumsPerCycle !! (turn `mod` turnsPerCycle)

execute 1 f = f
execute n f = f . execute (n-1) f

part3 (turns, wheels) = (max' - c , min' - c)
  where
    (max', min', _) = h 256 [0 | _ <- turns] Map.empty
    c = coinsOnTurn [0 | _ <- turns]
    h 0 xs ma = 
      let
        c = coinsOnTurn xs
        ma' = Map.insert (0, xs) (c, c) ma
      in (c, c, ma)

    h n xs ma =
      let
        n0 = nextTurn0 xs
        n1 = nextTurn1 xs
        n2 = nextTurn2 xs
        (max0, min0, ma')   = aux n0 ma   n
        (max1, min1, ma'')  = aux n1 ma'  n
        (max2, min2, ma''') = aux n2 ma'' n
        c = coinsOnTurn xs
        max_ = max (max max0 max1) max2 + c
        min_ = min (min min0 min1) min2 + c
        ma'''' = Map.insert (n, xs) (max_, min_) ma'''
      in (max_, min_, ma'''')
      where
        aux xs ma n =
          let
            l = Map.lookup (n-1, xs) ma
            (max', min') = fromMaybe (0,0) l
          in if isJust l then (max', min', ma) else h (n-1) xs ma

    wheelsTurns = zip turns wheels
    nextTurnWith n ts = map (\((t, w), x) -> (x+n+t) `mod` (length w)) $ zip wheelsTurns ts
    nextTurn0 = nextTurnWith 0
    nextTurn1 = nextTurnWith 1
    nextTurn2 = nextTurnWith (-1)

    coinsOnTurn xs =
      let
        f xs
          | length xs >= 3 = 1 + (length xs - 3)
          | otherwise      = 0
      in sum . map f . group . sort . concat . map (\(i, w) -> w !! i) $ zip xs wheels