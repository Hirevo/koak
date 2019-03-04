{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Parser.Lib where

import Control.Applicative
import Control.Monad

data ParseError =
    NotMatched String
    | EndOfInput
    deriving (Eq)
instance Show ParseError where
    show = \case
        NotMatched err -> err
        EndOfInput -> "Unexpected end of input"

data ParseResult a =
    Parsed a
    | NotParsed Pos ParseError
    deriving (Eq)
instance Show a => Show (ParseResult a) where
    show = \case
        Parsed a -> show a
        NotParsed pos err -> "ParseError (at " <> show pos <> "): " <> show err

instance Functor ParseResult where
    fmap f = \case
        NotParsed pos err -> NotParsed pos err
        Parsed x -> Parsed (f x)

instance Applicative ParseResult where
    pure = Parsed
    NotParsed pos err <*> _ = NotParsed pos err
    Parsed f          <*> x = fmap f x

instance Alternative ParseResult where
    empty = NotParsed Pos{ file = Nothing, line = 0, column = 0 } $ NotMatched "Unknown error"
    Parsed a       <|> _              = Parsed a
    NotParsed _  _ <|> Parsed b       = Parsed b
    NotParsed p1 a <|> NotParsed p2 b =
        if p1 > p2
            then NotParsed p1 a
            else NotParsed p2 b

instance Monad ParseResult where
    return = pure
    NotParsed pos err >>= _ = NotParsed pos err
    Parsed x          >>= f = f x

data Pos = Pos {
    line :: !Int,
    column :: !Int,
    file :: Maybe FilePath
} deriving (Eq)
instance Show Pos where
    show = \case
        Pos{ file = Just file, line, column } -> file <> ":" <> show line <> ":" <> show column
        Pos{ file = Nothing, line, column } -> show line <> ":" <> show column
instance Ord Pos where
    compare Pos{ line = l1, column = c1 } Pos{ line = l2, column = c2 } =
        compare l1 l2 <> compare c1 c2
nullPos :: Pos
nullPos = Pos 0 0 Nothing

data Range = Range {
    start :: !Pos,
    end :: !Pos
} deriving (Eq, Ord)
instance Show Range where
    show Range{ start, end } = show start <> "-" <> show end
nullRange :: Range
nullRange = Range nullPos nullPos

newtype Parser a = Parser
    ((String, Pos) -> ParseResult (a, (String, Pos)))

parse :: Parser a -> String -> ParseResult (a, (String, Pos))
parse (Parser f) input = f (input, Pos{ file = Nothing, line = 1, column = 1 })

parseFile :: Parser a -> String -> IO (ParseResult (a, (String, Pos)))
parseFile (Parser f) input = do
    contents <- readFile input
    return $ f (contents, Pos{ file = Just input, line = 1, column = 1 })

parseFrom :: Parser a -> (String, Pos) -> ParseResult (a, (String, Pos))
parseFrom (Parser f) = f

instance Functor Parser where
    fmap f (Parser p) = Parser $ \str -> do
        (a, str') <- p str
        return (f a, str')

instance Applicative Parser where
    pure a = Parser $ \str -> Parsed (a, str)
    Parser p1 <*> p2 = Parser $ \str -> do
        (f, str') <- p1 str
        parseFrom (fmap f p2) str'

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus

instance Monad Parser where
    return = pure
    Parser a >>= f = Parser $ \str -> do
        (ret, str') <- a str
        parseFrom (f ret) str'

instance MonadPlus Parser where
    mzero = Parser $ \(_, pos) -> NotParsed pos $ NotMatched "Unknown Error"
    mplus (Parser a) (Parser b) = Parser $ \str -> a str <|> b str

(??) :: Parser a -> String -> Parser a
Parser f ?? err = Parser $ \str ->
    case f str of
        Parsed ret -> return ret
        NotParsed pos _ -> NotParsed pos (NotMatched err)
infixl 0 ??

updatePos :: Char -> Pos -> Pos
updatePos '\n' pos = pos { line = line pos + 1, column = 1 }
updatePos '\t' pos = pos { column = column pos + 8 - ((column pos - 1) `mod` 8) }
updatePos _ pos = pos { column = column pos + 1 }

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
    (a, _) <- parseFrom p str
    return (a, str)

whileNot :: Parser a -> Parser String
whileNot p = Parser $ \str ->
    case parseFrom (peek p) str of
        Parsed (_, rest) -> Parsed ([], rest)
        NotParsed _ (NotMatched _) ->
            let recur = ((:) <$> pAny <*> whileNot p)
            in parseFrom recur str
        NotParsed pos EndOfInput -> NotParsed pos EndOfInput

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
    ([], pos) -> NotParsed pos EndOfInput
    (c:cs, pos) -> if f c
        then Parsed (c, (cs, updatePos c pos))
        else NotParsed pos $ NotMatched $ "Unexpected token: " <> show c

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
pEnd = Parser $ \case
    ([], pos) -> Parsed ((), ("", pos))
    (c:_, pos) -> NotParsed pos $ NotMatched $ "Expected end of input, got: " <> show c

pAny :: Parser Char
pAny = Parser $ \case
    ([], pos) -> NotParsed pos EndOfInput
    (c:cs, pos) -> Parsed (c, (cs, updatePos c pos))

pNothing :: Parser ()
pNothing = pure ()

pRange :: Char -> Char -> Parser Char
pRange c1 c2 = Parser $ \case
    ([], pos) -> NotParsed pos EndOfInput
    (c:cs, pos) | c1 <= c && c <= c2 -> Parsed (c, (cs, updatePos c pos))
    (c:_, pos) -> NotParsed pos $ NotMatched $ "Expected: [" <> [c1] <> "-" <> [c2] <> "], got: " <> show c

pChar, pNotChar :: Char -> Parser Char
pChar c = Parser $ \case
    ([], pos) -> NotParsed pos EndOfInput
    (x:xs, pos) | x == c -> Parsed (c, (xs, updatePos x pos))
    (x:_, pos) -> NotParsed pos $ NotMatched $ "Expected: " <> show c <> ", got: " <> show x
pNotChar c = Parser $ \case
    ([], pos) -> NotParsed pos EndOfInput
    (x:xs, pos) | x /= c -> Parsed (c, (xs, updatePos x pos))
    (x:_, pos) -> NotParsed pos $ NotMatched $ "Expected anything except: " <> show c <> ", got: " <> show x

pAnyOf :: String -> Parser Char
pAnyOf = choice . map pChar

pString :: String -> Parser String
pString = mapM pChar

pNot :: Parser a -> Parser ()
pNot p = Parser $ \str ->
    case parseFrom (peek p) str of
        Parsed (_, (_, pos)) -> NotParsed pos (NotMatched "Invalid input")
        NotParsed _ (NotMatched _) -> Parsed ((), str)
        NotParsed pos EndOfInput -> NotParsed pos EndOfInput
