import Data.List
import Data.List.Split
import Quest_4_input

accDiffs xs value = foldr (\x acc -> acc + abs(x - value)) 0 xs

part12 xs = accDiffs xs (minimum xs)

part3 xs = accDiffs xs ((sort xs) !! (length xs `div` 2))