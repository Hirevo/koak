module Parser.Lang where

import Control.Applicative
import Parser.Lib
import Data.Char (isSpace)
import Data.List (intersperse)

lower :: Parser Char
lower = pRange 'a' 'z'

upper :: Parser Char
upper = pRange 'A' 'Z'

digit :: Parser Char
digit = pRange '0' '9'

pIdent :: Parser String
pIdent = (:)
    <$> (lower <|> upper)
    <*> many (lower <|> upper <|> digit)

pInt :: Parser Int
pInt =
    let pDigit = pRange '0' '9'
        readInt = read :: String -> Int
        signedInt = (:) <$> pAnyOf "+-" <*> some pDigit
        unsignedInt = some pDigit
    in readInt <$> (signedInt <|> unsignedInt)

pFloat :: Parser Float
pFloat =
    let pDigit = pRange '0' '9'
        pDot = pChar '.'
        pSign = pAnyOf "+-"
        intPart = ((:) <$> pSign <*> some pDigit) <|> some pDigit
        fracPart = ((:) <$> pDot <*> some pDigit)
        complexFloat = ((++) <$> intPart <*> fracPart)
        simpleFloat = intPart
        readFloat = read :: String -> Float
    in readFloat <$> (complexFloat <|> simpleFloat)

pSpacing :: Parser String
pSpacing = some $ pAnyOf " \t\n"

pOptSpacing :: Parser String
pOptSpacing = many $ pAnyOf " \t\n"
