{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Parser.Lib where

import Control.Applicative
import Control.Monad

data ParseError =
    NotMatched
    | EndOfInput
    deriving (Eq)
data ParseResult a =
    Parsed a
    | NotParsed Pos ParseError
    deriving (Show, Eq)

instance Show ParseError where
    show NotMatched = "Invalid expression"
    show EndOfInput = "Unexpected end of input"

instance Functor ParseResult where
    fmap _ (NotParsed pos err) = NotParsed pos err
    fmap f (Parsed x)          = Parsed $ f x

instance Applicative ParseResult where
    pure = Parsed
    NotParsed pos err <*> _ = NotParsed pos err
    Parsed f          <*> x = f <$> x

instance Alternative ParseResult where
    empty = NotParsed Pos{ line = 0, column = 0 } NotMatched
    Parsed a <|> _ = Parsed a
    _        <|> b = b

instance Monad ParseResult where
    NotParsed pos err >>= _ = NotParsed pos err
    Parsed x          >>= f = f x

data Pos = Pos {
    line :: Int,
    column :: Int
} deriving (Eq, Ord)
instance Show Pos where
    show Pos{ line, column } = show line ++ ":" ++ show column

data Range = Range {
    start :: Pos,
    end :: Pos
} deriving (Show, Eq, Ord)

updatePos :: Char -> Pos -> Pos
updatePos '\n' pos = pos { line = line pos + 1, column = 0 }
updatePos '\t' pos = pos { column = column pos + 8 - ((column pos - 1) `mod` 8) }
updatePos _ pos = pos { column = column pos + 1 }

newtype Parser a = Parser {
    parse :: (String, Pos) -> ParseResult (a, (String, Pos))
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
    empty                 = Parser $ \_ -> empty
    Parser a <|> Parser b = Parser $ \str -> a str <|> b str

instance Monad Parser where
    Parser a >>= f = Parser $ \str -> do
        (ret, str') <- a str
        parse (f ret) str'

instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

currentPos :: Parser Pos
currentPos = Parser $ \(str, pos) -> Parsed (pos, (str, pos))

withRange :: Parser a -> Parser (a, Range)
withRange p = do
    start <- currentPos
    ret <- p
    end <- currentPos
    return (ret, Range { start, end })

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
        NotParsed _ NotMatched ->
            let recur = ((:) <$> pAny <*> whileNot p)
            in parse recur str
        NotParsed pos EndOfInput -> NotParsed pos EndOfInput

satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
    let consumeChar :: ((String, Pos) -> ParseResult (Char, (String, Pos)))
        consumeChar ([], pos) = NotParsed pos EndOfInput
        consumeChar (x:xs, pos)
            | f x = Parsed (x, (xs, updatePos x pos))
            | otherwise = NotParsed pos NotMatched
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
    let consumeEnd :: ((String, Pos) -> ParseResult ((), (String, Pos)))
        consumeEnd ([], pos) = Parsed ((), ("", pos))
        consumeEnd (_, pos)  = NotParsed pos NotMatched
    in Parser consumeEnd

pAny :: Parser Char
pAny =
    let consumeAny :: ((String, Pos) -> ParseResult (Char, (String, Pos)))
        consumeAny ([], pos) = NotParsed pos EndOfInput
        consumeAny (x:xs, pos) = Parsed (x, (xs, updatePos x pos))
    in Parser consumeAny

pNothing :: Parser ()
pNothing = pure ()

pRange :: Char -> Char -> Parser Char
pRange c =
    let consumeRange :: Char -> Char -> ((String, Pos) -> ParseResult (Char, (String, Pos)))
        consumeRange _  _ ([], pos) = NotParsed pos EndOfInput
        consumeRange c1 c2 (x:xs, pos)
            | c1 <= x && x <= c2 = Parsed (x, (xs, updatePos x pos))
            | otherwise          = NotParsed pos NotMatched
    in Parser . consumeRange c

pChar :: Char -> Parser Char
pChar =
    let consumeChar :: Char -> ((String, Pos) -> ParseResult (Char, (String, Pos)))
        consumeChar _ ([], pos) = NotParsed pos EndOfInput
        consumeChar c (x:xs, pos)
            | x == c    = Parsed (c, (xs, updatePos x pos))
            | otherwise = NotParsed pos NotMatched
    in Parser . consumeChar

pNotChar :: Char -> Parser Char
pNotChar =
    let consumeNotChar :: Char -> ((String, Pos) -> ParseResult (Char, (String, Pos)))
        consumeNotChar _ ([], pos) = NotParsed pos EndOfInput
        consumeNotChar c (x:xs, pos)
            | x == c    = NotParsed pos NotMatched
            | otherwise = Parsed (x, (xs, updatePos x pos))
    in Parser . consumeNotChar

pAnyOf :: String -> Parser Char
pAnyOf = choice . map pChar

pString :: String -> Parser String
pString = sequence . map pChar
