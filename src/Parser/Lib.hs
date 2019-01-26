module Parser.Lib where

import Control.Applicative
import Control.Monad

data ParseError =
    NotMatched
    | EndOfInput
    deriving (Eq)
data ParseResult a =
    Parsed a
    | NotParsed ParseError
    deriving (Show, Eq)

instance Show ParseError where
    show NotMatched = "Invalid expression"
    show EndOfInput = "Unexpected end of input"

instance Functor ParseResult where
    fmap _ (NotParsed err) = NotParsed err
    fmap f (Parsed x)      = Parsed $ f x

instance Applicative ParseResult where
    pure = Parsed
    NotParsed err <*> _ = NotParsed err
    Parsed f      <*> x = f <$> x

instance Alternative ParseResult where
    empty = NotParsed NotMatched
    Parsed a <|> _ = Parsed a
    _        <|> b = b

instance Monad ParseResult where
    NotParsed err >>= _ = NotParsed err
    Parsed x      >>= f = f x

newtype Parser a = Parser {
    parse :: String -> ParseResult (a, String)
}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \str -> do
        (a, str') <- p str
        return (f a, str')

instance Applicative Parser where
    pure a = Parser $ \str -> Parsed (a, str)
    Parser p1 <*> p2 = Parser $ \str -> do
        (f, str') <- p1 str
        parse (f <$> p2) str'

instance Alternative Parser where
    empty                 = Parser $ \_ -> NotParsed NotMatched
    Parser a <|> Parser b = Parser $ \str -> a str <|> b str

instance Monad Parser where
    Parser a >>= f = Parser $ \str -> do
        (ret, str') <- a str
        parse (f ret) str'

instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

-- |This function "peeks" if the following input would match the given parser.
-- It takes any parser and returns the same one but made non-consuming.
peek :: Parser a -> Parser a
peek p = Parser $ \str -> do
    (a, _) <- parse p str
    return (a, str)

whileNot :: Parser a -> Parser String
whileNot p = Parser $ \str ->
    case parse (peek p) str of
        Parsed (_, rest) -> Parsed ([], rest)
        NotParsed NotMatched ->
            let recur = ((:) <$> pAny <*> whileNot p)
            in parse recur str
        NotParsed EndOfInput -> NotParsed EndOfInput

satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
    let consumeChar :: (String -> ParseResult (Char, String))
        consumeChar [] = NotParsed EndOfInput
        consumeChar (x:xs)
            | f x = Parsed (x, xs)
            | otherwise = NotParsed NotMatched
    in Parser consumeChar

choice :: [Parser a] -> Parser a
choice = foldl (<|>) empty

fallback :: a -> Parser a -> Parser a
fallback a p = p <|> pure a

sepBy1 :: Parser b -> Parser a -> Parser [a]
sepBy1 sep p = (:) <$> p <*> many (sep *> p)

sepBy0 :: Parser b -> Parser a -> Parser [a]
sepBy0 sep = fallback [] . sepBy1 sep

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

both :: Parser a -> Parser b -> Parser (a, b)
both p1 p2 = (\a b -> (a, b)) <$> p1 <*> p2

pEnd :: Parser ()
pEnd =
    let consumeEnd :: (String -> ParseResult ((), String))
        consumeEnd [] = Parsed ((), "")
        consumeEnd _  = NotParsed NotMatched
    in Parser consumeEnd

pAny :: Parser Char
pAny =
    let consumeAny :: (String -> ParseResult (Char, String))
        consumeAny [] = NotParsed EndOfInput
        consumeAny (x:xs) = Parsed (x, xs)
    in Parser consumeAny

pNothing :: Parser ()
pNothing = pure ()

pRange :: Char -> Char -> Parser Char
pRange c =
    let consumeRange :: Char -> Char -> (String -> ParseResult (Char, String))
        consumeRange _  _ [] = NotParsed EndOfInput
        consumeRange c1 c2 (x:xs)
            | c1 <= x && x <= c2 = Parsed (x, xs)
            | otherwise          = NotParsed NotMatched
    in Parser . consumeRange c

pChar :: Char -> Parser Char
pChar =
    let consumeChar :: Char -> (String -> ParseResult (Char, String))
        consumeChar _ [] = NotParsed EndOfInput
        consumeChar c (x:xs)
            | x == c    = Parsed (c, xs)
            | otherwise = NotParsed NotMatched
    in Parser . consumeChar

pNotChar :: Char -> Parser Char
pNotChar =
    let consumeNotChar :: Char -> (String -> ParseResult (Char, String))
        consumeNotChar _ [] = NotParsed EndOfInput
        consumeNotChar c (x:xs)
            | x == c    = NotParsed NotMatched
            | otherwise = Parsed (x, xs)
    in Parser . consumeNotChar

pAnyOf :: String -> Parser Char
pAnyOf = choice . map pChar

pString :: String -> Parser String
pString = sequence . map pChar
