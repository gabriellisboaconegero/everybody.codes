{-# LANGUAGE ScopedTypeVariables #-}
import Data.List
import Data.Char
import Data.Maybe
import Quest_3_input
import qualified Data.Map as Map

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

-- inv moduli, for prime numbers
inv :: Int -> Int -> Int
inv a m = fastExpM a (m-2) m

-- Chinese remainder theorem
-- (Int, Int) =: (a, m)
crt :: [(Int, Int)] -> Int
crt congs = foldr innerCRT 0 congs
  where
    _M = product $ map snd congs
    innerCRT (a_i, m_i) acc =
      let
        _M_i = _M `div` m_i
        _N_i = inv _M_i m_i
      in (acc + ((((a_i `mod` _M) * (_M_i `mod` _M)) `mod` _M) * (_N_i `mod` _M)) `mod` _M) `mod` _M


part1 input = sum $ map (\(x, y) -> x + (100 * y)) backToxy
  -- size = x+y-1
  -- y = size-x+1
  -- pos = x-1
  -- x = pos+1
  where
    offAndSize = map (\(x, y) -> (x-1, x+y-1)) input
    newPosAndSize = map (\(off, size) -> ((off + 100) `mod` size, size)) offAndSize
    backToxy = map (\(newPos, size) -> (newPos+1, size-(newPos+1)+1)) newPosAndSize

part2 input = aux 0 1 c1
  where
    aux d step [] = d
    aux d step (x@(off, size):xs)
      | ((d - off) `mod` size + size) `mod` size == 0 = aux d (lcm step size) xs
      | otherwise = aux (d+step) step (x:xs)
    c1 = map (\(x, y) -> (y-1, x+y-1)) input
    backToxy = map (\(newPos, size) -> (newPos+1, size-(newPos+1)+1))