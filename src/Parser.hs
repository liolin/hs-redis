module Parser
  ( Parser
  , runParser
  , string
  , digit
  )
where 

import Control.Applicative (Alternative (..))
import Data.List (nub)
import Data.Char (isDigit)


data Error i e
  = EndOfInput
  | Unexpected i
  | CustomError e
  | Empty
  deriving (Eq, Show)


-- i: Input stream for example Char ([Char] is the same as String)
-- e: Custom Error Message, if none use Void
-- a: The result of the parsing function

newtype Parser i e a = Parser
  {
    runParser :: [i] -> Either [Error i e] (a, [i])
  }


instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Left err -> Left err
      Right (output, rest) -> Right (f output, rest)

instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)
  Parser f <*> Parser p = Parser $ \input ->
    case f input of
      Left err -> Left err
      Right (f', rest) ->
        case p rest of
          Left err -> Left err
          Right (output, rest') -> Right (f' output, rest')

instance Monad (Parser i e) where
  return = pure

  Parser p >>= k = Parser $ \input ->
    case p input of
      Left err -> Left err
      Right (output, rest) ->
        let
          Parser p' = k output
        in
        p' rest

instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ \_ -> Left [Empty]
  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Right (output, rest) -> Right (output, rest)
      Left err -> case r input of
        Left err' -> Left $ nub $ err <> err' 
        Right (output, rest) -> Right (output, rest)


-- Most primitive function for a parser
satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left [EndOfInput]
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise -> Left [Unexpected hd]


char :: Eq i => i -> Parser i e i
char i = satisfy (== i)

digit :: Parser Char e Char 
digit = satisfy isDigit

string :: Eq i => [i] -> Parser i e [i]
string = traverse char

number :: Parser Char e Char
number = do
  d <- digit
  ds <- number
  return (d : ds)
