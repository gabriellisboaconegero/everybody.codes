import Data.List
import Data.List.Split
import Quest_1_input

potionsRequiredPerMonster :: Char -> Int
potionsRequiredPerMonster a = case a of
  'A' -> 0
  'x' -> 0
  'B' -> 1
  'C' -> 3
  'D' -> 5
  _   -> -1

extraPotionPerMonster :: Char -> Int
extraPotionPerMonster a = case a of
  'x' -> 0
  _   -> 1

potionsRequiredPerGroup :: [Char] -> Int
potionsRequiredPerGroup g = extraPotions + (sum . map potionsRequiredPerMonster $ g)
  where
    extraPotionSum = sum . map extraPotionPerMonster $ g
    extraPotions   = extraPotionSum * (extraPotionSum - 1)

part1 :: String -> Int
part1 = sum . map potionsRequired
  where
    potionsRequired = potionsRequiredPerMonster

part2 :: String -> Int
part2 = sum . map potionsRequired . chunksOf 2
  where
    potionsRequired = potionsRequiredPerGroup

part3 :: String -> Int
part3 = sum . map potionsRequired . chunksOf 3
  where
    potionsRequired = potionsRequiredPerGroup