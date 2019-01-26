{-# LANGUAGE DeriveFunctor #-}
module Parser.Lang where

import Control.Applicative
import Parser.Lib
import Data.Char (isSpace)
import Data.List (sortBy)
import Control.Monad.State.Lazy
import Misc
import Debug.Trace

data PrecMap = PrecMap {
    bin_prec :: [(String, Int)],
    un_prec :: [(String, Int)]
} deriving (Show, Eq)

type KoakParser = StateT PrecMap Parser

newtype ID a = ID a deriving (Show, Eq, Functor)

intoParser :: PrecMap -> KoakParser a -> Parser a
intoParser = flip evalStateT

type AST = [Stmt]
data Stmt =
    DefnStmt Defn
    | ExprStmt Expr
    deriving (Show, Eq)
data Type =
    IntType
    | FloatType
    | VoidType
    | ArrType
    deriving (Show, Eq)
data Arg = Arg {
    arg_name :: String,
    arg_type :: Type
} deriving (Show, Eq)
data Arity =
    Unary
    | Binary
    deriving (Show, Eq)
data Defn =
    Op OpDefn
    | Fn FnDefn
    deriving (Show, Eq)
data OpDefn = OpDefn {
    opdefn_op :: String,
    opdefn_prec :: Int,
    opdefn_arity :: Arity,
    opdefn_args :: [Arg],
    opdefn_body :: Expr
} deriving (Show, Eq)
data FnDefn = FnDefn {
    fndefn_name :: String,
    fndefn_args :: [Arg],
    fndefn_body :: Expr
} deriving (Show, Eq)
data Expr =
    Call CallExpr
    | Bin BinExpr
    | Un UnExpr
    | Lit Literal
    | Ident String
    | For ForExpr
    | If IfExpr
    | While WhileExpr
    | Simple [Expr]
    deriving (Show, Eq)
data ForExpr = ForExpr {
    for_it_ident :: Expr,
    for_it_init :: Expr,
    for_cond_ident :: Expr,
    for_cond_expr :: Expr,
    for_op_expr :: Expr,
    for_body :: Expr
} deriving (Show, Eq)
data IfExpr = IfExpr {
    if_cond :: Expr,
    if_then :: Expr,
    if_else :: Maybe Expr
} deriving (Show, Eq)
data WhileExpr = WhileExpr {
    while_cond :: Expr,
    while_body :: Expr
} deriving (Show, Eq)
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
    bin_op :: String,
    bin_lhs :: Expr,
    bin_rhs :: Expr
} deriving (Show, Eq)
data UnExpr = UnExpr {
    un_op :: String,
    un_arg :: Expr
} deriving (Show, Eq)

defaultPrecedenceMap :: PrecMap
defaultPrecedenceMap = PrecMap {
    bin_prec = [ ( "*", 50), ( "/", 50)
               , ( "+", 40), ( "-", 40)
               , ( "<", 30), ( ">", 30)
               , ("==", 20), ("!=", 20)
               , ( "=", 10) ],
    un_prec  = [ ("-", 30), ("!", 30) ]
}

lookupAll :: (k -> Bool) -> [(k, v)] -> [(k, v)]
lookupAll f = filter $ f . fst

binOp :: KoakParser (String, Int)
binOp = do
    precMap <- get
    precMap |> bin_prec
            |> sortBy (\(a, _) (b, _) -> compare (length a) (length b))
            |> reverse
            |> map (\(a, b) -> const (a, b) <$> pString a)
            |> choice
            |> lift

unOp :: KoakParser (String, Int)
unOp = do
    precMap <- get
    precMap |> un_prec
            |> sortBy (\(a, _) (b, _) -> compare (length a) (length b))
            |> reverse
            |> map (\(a, b) -> const (a, b) <$> pString a)
            |> choice
            |> lift

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

dot :: Parser Char
dot = pChar '.'

identifier :: Parser String
identifier = (:)
    <$> (lower <|> upper)
    <*> many (lower <|> upper <|> digit)

typeName :: Parser Type
typeName =
    (const IntType <$> pString "int")
    <|> (const FloatType <$> pString "double")
    <|> (const VoidType <$> pString "void")

integer :: Parser Int
integer = do
    digits <- some digit
    return $ readInt digits

decimalConst :: Parser Literal
decimalConst = IntLiteral <$> integer

doubleConst :: Parser Literal
doubleConst =
    let p1 = do
            intPart <- some digit
            dot <- dot
            decPart <- fallback "0" $ some digit
            return $ FloatLiteral $ readFloat (intPart ++ (dot : decPart))
        p2 = do
            dot <- dot
            decPart <- some digit
            return $ FloatLiteral $ readFloat ('0' : dot : decPart)
    in p1 <|> p2

literal :: Parser Literal
literal =
    doubleConst
    <|> decimalConst

spacing :: Parser String
spacing = some $ satisfy isSpace

optSpacing :: Parser String
optSpacing = many $ satisfy isSpace

expressions :: KoakParser Expr
expressions =
    (For <$> forExpression)
    <|> (If <$> ifExpr)
    <|> (While <$> whileExpr)
    <|> simpleExpr

forExpression :: KoakParser ForExpr
forExpression = do
    lift optSpacing
    lift $ pString "for"
    lift spacing
    it_name <- lift (Ident <$> identifier)
    lift optSpacing
    lift $ pString "="
    lift optSpacing
    it_init <- expression
    lift optSpacing
    lift $ pChar ','
    lift optSpacing
    cond_name <- lift (Ident <$> identifier)
    lift optSpacing
    lift $ pChar '<'
    lift optSpacing
    cond_expr <- expression
    lift optSpacing
    lift $ pChar ','
    lift optSpacing
    op_expr <- expression
    lift optSpacing
    lift $ pString "in"
    lift optSpacing
    body <- expressions
    return $ ForExpr {
        for_it_ident = it_name,
        for_it_init = it_init,
        for_cond_ident = cond_name,
        for_cond_expr = cond_expr,
        for_op_expr = op_expr,
        for_body = body
    }

ifExpr :: KoakParser IfExpr
ifExpr = do
    lift optSpacing
    lift $ pString "if"
    lift spacing
    if_expr <- expression
    lift spacing
    lift $ pString "then"
    lift spacing
    then_body <- expressions
    precMap <- get
    else_body <- lift $ optional $ do
        spacing
        pString "else"
        spacing
        evalStateT expressions precMap
    return $ IfExpr {
        if_cond = if_expr,
        if_then = then_body,
        if_else = else_body
    }

whileExpr :: KoakParser WhileExpr
whileExpr = do
    lift optSpacing
    lift $ pString "while"
    lift spacing
    while_expr <- expression
    lift spacing
    lift $ pString "do"
    lift spacing
    body <- expressions
    return $ WhileExpr {
        while_cond = while_expr,
        while_body = body
    }

simpleExpr :: KoakParser Expr
simpleExpr = do
    precMap <- get
    let pExpr = evalStateT expression precMap
    ret <- lift $ sepBy1 (pChar ':') pExpr
    let (x:xs) = ret
    return $ if null xs then x else Simple (x:xs)

parseBinOp :: Expr -> Int -> KoakParser Expr
parseBinOp lhs minPrec = do
    s <- get
    let pBinOp = evalStateT binOp s
    let pUnary = evalStateT unary s
    let outer lhs minPrec = fallback lhs $ do
         optSpacing
         (_, prec) <- peek pBinOp
         if prec < minPrec then empty else fallback lhs $ do
             optSpacing
             (op, prec) <- pBinOp
             optSpacing
             rhs <- pUnary
             let expr = Bin $ BinExpr { bin_op = op, bin_lhs = lhs, bin_rhs = rhs }
             let inner rhs' = fallback rhs' $ do
                  optSpacing
                  (_, prec') <- peek pBinOp
                  if prec' <= prec then empty else fallback rhs' $ do
                     let pBinExpr = evalStateT (parseBinOp rhs' prec') s
                     optSpacing
                     rhs <- pBinExpr
                     inner rhs
             lhs <- fallback expr $ do
                rhs <- inner rhs
                return $ Bin $ BinExpr { bin_op = op, bin_lhs = lhs, bin_rhs = rhs }
             outer lhs minPrec
    lift $ outer lhs minPrec

expression :: KoakParser Expr
expression = do
    precMap <- get
    lift optSpacing
    lhs <- unary
    parseBinOp lhs 0

unary :: KoakParser Expr
unary = do
    precMap <- get
    lift optSpacing
    let p1 = do
         operator <- lift $ choice $ map (pString . fst) $ un_prec precMap
         lift optSpacing
         operand <- unary
         return $ Un $ UnExpr {
             un_op = operator,
             un_arg = operand
         }
    p1 <|> postfix

postfix :: KoakParser Expr
postfix = do
    precMap <- get
    lift optSpacing
    let ident = lift $ Ident <$> identifier
    let lit = lift $ Lit <$> literal
    let paren = do
         lift $ pChar '('
         lift optSpacing
         expr <- expressions
         lift optSpacing
         lift $ pChar ')'
         return expr
    let primary = ident <|> lit <|> paren
    let callExpr = do
         name <- lift identifier
         lift optSpacing
         args <- do
            lift $ pChar '('
            precMap <- get
            let pExpr = evalStateT expression precMap
            args <- lift $ sepBy0 (optSpacing *> pChar ',') (optSpacing *> pExpr)
            lift optSpacing
            lift $ pChar ')'
            return args
         return $ Call $ CallExpr {
             call_ident = name,
             call_args = args
         }
    callExpr <|> primary

argument :: Parser Arg
argument = do
    name <- optSpacing *> identifier
    optSpacing *> pChar ':'
    ty <- optSpacing *> typeName
    return $ Arg {
        arg_name = name,
        arg_type = ty
    }

defnUnaryStatment :: KoakParser OpDefn
defnUnaryStatment = do
    lift $ optSpacing *> pString "def"
    lift $ spacing *> pString "unary"
    name <- lift $ optSpacing *> whileNot (pAnyOf " \n\t" <|> lower <|> upper <|> digit)
    prec <- lift $ optSpacing *> integer
    arg <- lift $ between
        (optSpacing *> pChar '(')
        (optSpacing *> pChar ')')
        argument
    lift $ optSpacing *> pChar ':'
    ty <- lift $ optSpacing *> typeName
    lift optSpacing
    exprs <- expressions
    lift $ optSpacing *> pChar ';'
    s <- get
    put $ PrecMap {
        un_prec = (name, prec) : un_prec s,
        bin_prec = bin_prec s
    }
    return $ OpDefn {
        opdefn_op = name,
        opdefn_prec = prec,
        opdefn_arity = Unary,
        opdefn_args = [arg],
        opdefn_body = exprs
    }

defnBinaryStatment :: KoakParser OpDefn
defnBinaryStatment = do
    lift $ optSpacing *> pString "def"
    lift $ spacing *> pString "binary"
    name <- lift $ optSpacing *> whileNot (pAnyOf " \n\t" <|> lower <|> upper <|> digit)
    prec <- lift $ optSpacing *> integer
    (arg1, arg2) <- lift $ between
        (optSpacing *> pChar '(')
        (optSpacing *> pChar ')')
        (both argument (optSpacing *> pChar ',' *> argument))
    lift $ optSpacing *> pChar ':'
    ty <- lift $ optSpacing *> typeName
    lift optSpacing
    exprs <- expressions
    lift $ optSpacing *> pChar ';'
    s <- get
    put $ PrecMap {
        un_prec = un_prec s,
        bin_prec = (name, prec) : bin_prec s
    }
    return $ OpDefn {
        opdefn_op = name,
        opdefn_prec = prec,
        opdefn_arity = Binary,
        opdefn_args = [arg1, arg2],
        opdefn_body = exprs
    }

defnFuncStatment :: KoakParser FnDefn
defnFuncStatment = do
    lift $ optSpacing *> pString "def"
    name <- lift $ optSpacing *> identifier
    args <- lift $ between
        (optSpacing *> pChar '(')
        (optSpacing *> pChar ')')
        (sepBy0 (optSpacing *> pChar ',') argument)
    lift $ optSpacing *> pChar ':'
    ty <- lift $ optSpacing *> typeName
    lift optSpacing
    exprs <- expressions
    lift $ optSpacing *> pChar ';'
    return $ FnDefn {
        fndefn_name = name,
        fndefn_args = args,
        fndefn_body = exprs
    }

defnStatement :: KoakParser Defn
defnStatement =
    (Op <$> defnUnaryStatment)
    <|> (Op <$> defnBinaryStatment)
    <|> (Fn <$> defnFuncStatment)

exprStatement :: KoakParser Expr
exprStatement = do
    lift optSpacing
    expr <- expressions
    lift optSpacing
    lift $ pChar ';'
    return expr

statement :: KoakParser Stmt
statement =
    (DefnStmt <$> defnStatement)
    <|> (ExprStmt <$> exprStatement)

statements :: KoakParser [Stmt]
statements = do
    lift optSpacing
    stmt <- statement
    s <- get
    lift $ fallback [stmt] $ do
        stmts <- evalStateT statements s
        return (stmt : stmts)

program :: Parser AST
program = evalStateT allStatements defaultPrecedenceMap
    where
        allStatements :: KoakParser AST
        allStatements = do
            stmts <- statements
            lift $ optSpacing *> pEnd
            return stmts
