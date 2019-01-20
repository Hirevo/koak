module Parser.Lang where

import Control.Applicative
import Parser.Lib
import Data.Char (isSpace)
import Data.List (intersperse)

type AST = [Stmt]
data Stmt =
    DefnStmt Defn
    | ExprStmt Expr
    deriving (Show, Eq)
data Type =
    IntType
    | FloatType
    | VoidType
    deriving (Show, Eq)
data Arg = Arg {
    arg_name :: String,
    arg_type :: Type
} deriving (Show, Eq)
data Defn = Defn {
    def_name :: String,
    def_args :: [Arg],
    def_body :: Expr
} deriving (Show, Eq)
data Exprs =
    For ForExpr
    | If IfExpr
    | While WhileExpr
    | Simple Expr [Expr]
    deriving (Show, Eq)
data ForExpr = ForExpr {
    for_it_ident :: String,
    for_it_init :: Expr,
    for_cond_ident :: String,
    for_cond_expr :: Expr,
    for_op_expr :: Expr,
    for_body :: Exprs
} deriving (Show, Eq)
data IfExpr = IfExpr {
    if_cond :: Expr,
    if_then :: Exprs,
    if_else :: Maybe Exprs
} deriving (Show, Eq)
data WhileExpr = WhileExpr {
    while_cond :: Expr,
    while_body :: Exprs
} deriving (Show, Eq)
data Expr =
    Call CallExpr
    | Bin BinExpr
    | Un UnExpr
    | Lit Literal
    deriving (Show, Eq)
data Literal =
    FloatLiteral Float
    | IntLiteral Int
    | VoidLiteral
    deriving (Show, Eq)
data CallExpr = CallExpr {
    call_ident :: String,
    call_args :: [Expr]
} deriving (Show, Eq)
data BinExpr = BinExpr {
    bin_op :: BinOp,
    bin_lhs :: Expr,
    bin_rhs :: Expr
} deriving (Show, Eq)
data BinOp =
    Add | Sub | Mult | Div
    | Lt | Gt | Eq | Neq
    | Assign
    deriving (Show, Eq)
data UnExpr = UnExpr {
    un_op :: UnOp,
    un_arg :: Expr
} deriving (Show, Eq)
data UnOp =
    Not | Neg
    deriving (Show, Eq)

readInt :: String -> Int
readInt = read

readFloat :: String -> Float
readFloat = read
            
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

pInt :: Parser Literal
pInt = do
    digits <- some $ pRange '0' '9'
    return $ IntLiteral $ readInt digits

pFloat :: Parser Literal
pFloat =
    let p1 = do
            intPart <- some $ pRange '0' '9'
            dot <- pString "."
            decPart <- many $ pRange '0' '9'
            return $ FloatLiteral $ readFloat (intPart ++ dot ++ decPart)
        p2 = do
            dot <- pString "."
            decPart <- some $ pRange '0' '9'
            return $ FloatLiteral $ readFloat ('0' : dot ++ decPart)
    in p1 <|> p2

pLiteral :: Parser Literal
pLiteral =
    pFloat
    <|> pInt

pSpacing :: Parser String
pSpacing = some $ pAnyOf " \t\n"

pOptSpacing :: Parser String
pOptSpacing = many $ pAnyOf " \t\n"
