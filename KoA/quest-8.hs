import Data.List
import Data.List.Split
import Quest_8_input

-- bsearch :: Integral a => (a -> a) -> a -> a
bsearch f m = bsearch' 0 m
  where
    bsearch' a b
      | b < a      = b
      | m >= f mid = bsearch' (mid + 1) b
      | otherwise  = bsearch' a (mid - 1)
      where
        mid = a + (b - a) `div` 2

-- fun :: Integral a => a -> a
fun n = n * n + 2 * n + 1

base n = 2 * n + 1

square x = x * x

-- fastExpM :: Integral a => a -> a -> a -> a
fastExpM b x m
  | x == 0         = 1
  | x == 1         = b
  | x `mod` 2 == 1 = mo (res * b)
  | otherwise      = res
  where
    x2 = x `div` 2
    mo a = a `mod` m
    res = mo (square (fastExpM b x2 m))

-- somatory :: Integral a => a -> a -> (a -> a) -> [a]
somatory i n f = aux i n f 0
  where
    aux i n f acc
      | i > n     = [fi]
      | otherwise = fi : (aux (i + 1) n f fi)
        where
          fi = f i + acc

-- fun2 :: Integral a => a -> a -> a -> [a]
fun2 x fun = aux 0 0
  where
    aux i acc
      | next > x   = (i, acc)
      | otherwise = aux (i + 1) next
      where
        next = acc + fun i

part1 input = (uncurry (*)) ((base ceilHeight), diffTotal)
  where
    height = bsearch fun input
    ceilHeight = height + (if fun height /= input then 1 else 0)
    diffTotal = (fun ceilHeight) - input

-- part2 input modulus expBase = (uncurry (*)) ((base ceilHeight), diffTotal)
part2 input modulus expBase = (height, size, fun height + size)
  where
    fun i = (2 * i + 1) * (fastExpM expBase i modulus)
    (height, size) = fun2 input fun
    ceilHeight = height + (if size /= input then 0 else -1)
    diffTotal = (if size /= input then fun height else 0) + size - input

fun3 x input modulus = aux 1 1 [1]
  where
    aux i thickness xs
      | total > x  = (i, total, remove, total - x)
      | otherwise = aux (i + 1) thickness' xs'
      where
        thickness' = ((thickness * input) `mod` modulus) + modulus
        xs' = thickness' : (map ((+) thickness') xs)

        sum' = 2 * (sum xs') - sum (drop (length xs' - 1) xs')
        xs'' = drop 1 xs'
        remove = 2 * (sum . map (\x -> (input * (2 * length xs' - 1) * x) `mod` modulus) $ xs'') - (sum . map (\x -> (input * (2 * length xs' - 1) * x) `mod` modulus) $ (drop (length xs'' - 1) xs''))

        total = sum' - remove
