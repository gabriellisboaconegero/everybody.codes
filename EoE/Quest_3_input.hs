{-# LANGUAGE MultilineStrings #-}
module Quest_3_input where
import Parser
import Control.Applicative (some, (<|>))
import Data.List (nub, transpose, intersperse)
import Data.Char
import Data.Either

type Coord = (Int, Int)

sep0 = char '\n'
sep1 = char ' '
equals = char '='

nat :: Parser Char Int Int
nat  = read <$> natural

coord :: String -> Parser Char Int Int
coord co = string co *> equals *> nat

coords = (coord "x" <* sep1) <~> coord "y"

final :: Parser Char Int [Coord]
final = sepBy coords sep0 <* eof

run part = parse (final >>= (return . part))

-- ================== INPUTS ====================
example1 = """
x=1 y=2
x=2 y=3
x=3 y=4
x=4 y=4
"""

example2 = """
x=12 y=2
x=8 y=4
x=7 y=1
x=1 y=5
x=1 y=3
"""

example3 = """
x=3 y=1
x=3 y=9
x=1 y=5
x=4 y=10
x=5 y=3
"""

input1 = """
x=1 y=1
x=2 y=2
x=3 y=3
x=4 y=4
x=5 y=5
x=6 y=6
x=1 y=11
x=2 y=3
x=1 y=7
x=3 y=8
"""

input2 = """
x=19 y=49
x=14 y=6
x=3 y=1
x=20 y=10
x=1 y=31
x=16 y=2
x=23 y=19
x=34 y=20
x=8 y=16
x=3 y=3
"""

input3 = """
x=1 y=3
x=4 y=2
x=37 y=11
x=5 y=9
x=23 y=9
x=12 y=6
x=38 y=22
x=35 y=19
x=1 y=43
x=6 y=32
"""
