module Parser where
import Control.Applicative
import Data.List (nub)
import Data.Char

-- The error type in case the parsing fails
data Error i e
  = EndOfInput
  | Unexpected i
  | Expected i i
  | CustomError e
  | Empty
  deriving (Eq, Show)

-- The parser type itself.
-- A parser is a function that receives the input (stream)
-- consumes it contents following some rules and return the
-- value parsed and the rest of the input (stream). It can fail
-- so it returns a error or the value and the rest of input
--
-- i : input type (does not need to be a string, so we can parse bits)
-- So the library can be used to parsing file types.
--
-- Example: "6aaaaa aosakas"
-- Parse the number n then proced to parse n letters a
newtype Parser i e a = Parser
  { runParser :: [i] -> Either [Error i e] (a, [i])
  }

-- Given a predicate, check if first input symbol
-- satisfy the predicate
token :: (i -> Error i e) -> (i -> Bool) -> Parser i e i
token mkErr predicate = Parser $ \input ->
  case input of
    [] -> Left [EndOfInput]
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise    -> Left [mkErr hd]

satisfy :: (i -> Bool) -> Parser i e i
satisfy = token Unexpected

-- verifify if fist input symbol is equal to some symbol x
char :: Eq i => i -> Parser i e i
char x = token (Expected x) (== x)

string :: Eq i => [i] -> Parser i e [i]
string = traverse char

oneOf :: Eq i => [i] -> Parser i e i
oneOf r = token Unexpected (\x -> any (== x) r)

digit :: Parser Char e Int
digit = (\x -> ord(x) - ord('0')) <$> oneOf ['0'..'9']

natural :: Eq e => Parser Char e Int
natural = foldl (\acc x -> 10 * acc + x) 0 <$> (some digit)

exact :: Alternative f => Int -> f a -> f [a]
exact 0 p = pure []
exact n p = liftA2 (:) p (exact (n-1) p)

lookahead :: Parser i e a -> Parser i e a
lookahead (Parser p) = Parser $ \input -> do
  (x, _) <- p input
  return (x, input)

sepBy :: Alternative f => f a -> f b -> f [a]
sepBy p sep = liftA2 (:) p (many (sep *> p))

sepBy1 :: Alternative f => f a -> f b -> f [a]
sepBy1 p sep = liftA2 (:) p (some (sep *> p))

nFollowedBynAs :: Eq e => Parser Char e (Int, String)
nFollowedBynAs = do
  n <- natural
  ((,) n) <$> (exact n (char 'a'))

aFollowedByb :: Eq e => Parser Char e (String, String)
aFollowedByb = do
  strA <- some (char 'a')
  ((,) strA) <$> (exact (length strA) (char 'b'))

-- less than <*, <*>, *> so we can still chain but the <* is done first
infixl 3 <:>
(<:>) :: Applicative f => f a -> f b -> f (a, b)
p <:> q = liftA2 (,) p q

-- instance Functor (Parser i e) where
--   fmap f (Parser p) = Parser $ \input ->
--     case p input of
--       Left err -> Left err
--       Right (output, rest) -> Right (f output, rest)
--
-- using Either as monad and do notation
instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    return (f output, rest)

instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)

  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest1) <- f input
    (output, rest2) <- p rest1
    return (f' output, rest2)

instance Monad (Parser i e) where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ \_ -> Left [Empty]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)