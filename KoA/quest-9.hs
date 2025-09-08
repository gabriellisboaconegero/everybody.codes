import Data.List
import Data.List.Split
import Quest_9_input
import qualified Data.Map as Map

-- coinExchange :: [Int] -> Map.Map Int Int -> Int -> Int
coinExchange coins dp amount target
  | amount == target = dp'
  | otherwise        = coinExchange coins dp' (amount + 1) target
  where
    getDP :: Int -> Int
    getDP n = Map.findWithDefault (-2) n dp

    coins' = filter (\x -> x > -1) . map (\coin -> (getDP (amount - coin)) + 1) $ coins
    resultForAmount = if length coins' > 0 then minimum coins' else 0
    dp' = Map.insert amount resultForAmount dp

part1 coins input = sum . map (\coin -> Map.findWithDefault 0 coin coinsExchanges) $ input
  where
    mini = minimum coins
    coinsExchanges = coinExchange coins (Map.singleton 0 0) mini (maximum input)

part2 = part2

linearSearch exchanges target = aux lower upper target
  where
    upper = (target `div` 2)
    lower = (target `div` 2 - (50 - target `mod` 2))

    aux l u mi
      | l >= u = mi'
      | otherwise = aux (l+1) u mi'
      where
        mi' = min mi (g l + g (target - l))
        g x = Map.findWithDefault 0 x exchanges

part3 coins input = sum . map (linearSearch coinsExchanges) $ input
  where
    mini = minimum coins
    coinsExchanges = coinExchange coins (Map.singleton 0 0) mini ((maximum input) `div` 2 + 51)