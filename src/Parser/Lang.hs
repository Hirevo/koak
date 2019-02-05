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

newtype Untyped a = Untyped a deriving (Show, Eq, Functor)

intoParser :: PrecMap -> KoakParser a -> Parser a
intoParser = flip evalStateT

type AST a = [a (Stmt a)]
data Stmt a =
    DefnStmt (a (Defn a))
    | ExprStmt (a (Expr a))
data Type =
    IntType
    | FloatType
    | VoidType
data Arg = Arg {
    arg_name :: String,
    arg_type :: Type
}
data Arity =
    Unary
    | Binary
data Defn a =
    Op (a (OpDefn a))
    | Fn (a (FnDefn a))
data OpDefn a = OpDefn {
    opdefn_op :: String,
    opdefn_prec :: Int,
    opdefn_arity :: Arity,
    opdefn_args :: [a Arg],
    opdefn_ret_ty :: Type,
    opdefn_body :: a (Expr a)
}
data FnDefn a = FnDefn {
    fndefn_name :: String,
    fndefn_args :: [a Arg],
    fndefn_ret_ty :: Type,
    fndefn_body :: a (Expr a)
}
data Expr a =
    Call (a (CallExpr a))
    | Bin (a (BinExpr a))
    | Un (a (UnExpr a))
    | Lit (a Literal)
    | Ident (a String)
    | For (a (ForExpr a))
    | If (a (IfExpr a))
    | While (a (WhileExpr a))
data ForExpr a = ForExpr {
    for_init :: a (Expr a),
    for_cond :: a (Expr a),
    for_oper :: a (Expr a),
    for_body :: a (Expr a)
}
data IfExpr a = IfExpr {
    if_cond :: a (Expr a),
    if_then :: a (Expr a),
    if_else :: Maybe (a (Expr a))
}
data WhileExpr a = WhileExpr {
    while_cond :: a (Expr a),
    while_body :: a (Expr a)
}
data Literal =
    FloatLiteral Float
    | IntLiteral Int
    | VoidLiteral
data CallExpr a = CallExpr {
    call_ident :: String,
    call_args :: [a (Expr a)]
}
data BinExpr a = BinExpr {
    bin_op :: String,
    bin_lhs :: a (Expr a),
    bin_rhs :: a (Expr a)
}
data UnExpr a = UnExpr {
    un_op :: String,
    un_arg :: a (Expr a)
}

defaultPrecedenceMap :: PrecMap
defaultPrecedenceMap = PrecMap {
    bin_prec = [ ( "*", 50), ( "/", 50)
               , ( "+", 40), ( "-", 40)
               , ( "<", 30), ( ">", 30)
               , ("==", 20), ("!=", 20)
               , ( "=", 10)
               , ( ":",  1) ],
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
    <|> (const VoidLiteral <$> pString "()")

spacing :: Parser String
spacing = some (satisfy isSpace <|> (pString "--" *> whileNot (pChar '\n') *> pChar '\n'))

optSpacing :: Parser String
optSpacing = many (satisfy isSpace <|> (pString "--" *> whileNot (pChar '\n') *> pChar '\n'))

expressions :: KoakParser (Expr Untyped)
expressions =
    (For . Untyped <$> forExpression)
    <|> (If . Untyped <$> ifExpr)
    <|> (While . Untyped <$> whileExpr)
    <|> expression

forExpression :: KoakParser (ForExpr Untyped)
forExpression = do
    lift $ optSpacing *> pString "for"
    lift spacing
    init <- expression
    lift $ optSpacing *> pChar ','
    lift optSpacing
    cond <- expression
    lift $ optSpacing *> pChar ','
    lift optSpacing
    oper <- expression
    lift $ optSpacing *> pString "in"
    lift optSpacing
    body <- expressions
    return $ ForExpr {
        for_init = Untyped init,
        for_cond = Untyped cond,
        for_oper = Untyped oper,
        for_body = Untyped body
    }

ifExpr :: KoakParser (IfExpr Untyped)
ifExpr = do
    lift $ optSpacing *> pString "if"
    lift spacing
    if_expr <- expression
    lift $ spacing *> pString "then"
    lift spacing
    then_body <- expressions
    precMap <- get
    else_body <- lift $ optional $ do
        spacing
        pString "else"
        spacing
        evalStateT expressions precMap
    return $ IfExpr {
        if_cond = Untyped if_expr,
        if_then = Untyped then_body,
        if_else = Untyped <$> else_body
    }

whileExpr :: KoakParser (WhileExpr Untyped)
whileExpr = do
    lift $ optSpacing *> pString "while"
    lift spacing
    while_expr <- expression
    lift $ spacing *> pString "do"
    lift spacing
    body <- expressions
    return $ WhileExpr {
        while_cond = Untyped while_expr,
        while_body = Untyped body
    }

parseBinOp :: Expr Untyped -> Int -> KoakParser (Expr Untyped)
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
             let expr = Bin $ Untyped $ BinExpr { bin_op = op, bin_lhs = Untyped lhs, bin_rhs = Untyped rhs }
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
                return $ Bin $ Untyped $ BinExpr { bin_op = op, bin_lhs = Untyped lhs, bin_rhs = Untyped rhs }
             outer lhs minPrec
    lift $ outer lhs minPrec

expression :: KoakParser (Expr Untyped)
expression = do
    precMap <- get
    lift optSpacing
    lhs <- unary
    parseBinOp lhs 0

unary :: KoakParser (Expr Untyped)
unary = do
    precMap <- get
    lift optSpacing
    let p1 = do
         operator <- lift $ choice $ map (pString . fst) $ un_prec precMap
         lift optSpacing
         operand <- unary
         return $ Un $ Untyped $ UnExpr {
             un_op = operator,
             un_arg = Untyped operand
         }
    p1 <|> postfix

postfix :: KoakParser (Expr Untyped)
postfix = do
    precMap <- get
    lift optSpacing
    let ident = lift $ Ident . Untyped <$> identifier
    let lit = lift $ Lit . Untyped <$> literal
    let paren = do
         lift $ pChar '('
         lift optSpacing
         expr <- expressions
         lift $ optSpacing *> pChar ')'
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
         return $ Call $ Untyped $ CallExpr {
             call_ident = name,
             call_args = Untyped <$> args
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

defnUnaryStatment :: KoakParser (OpDefn Untyped)
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
    ret_ty <- lift $ optSpacing *> typeName
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
        opdefn_args = [Untyped arg],
        opdefn_ret_ty = ret_ty,
        opdefn_body = Untyped exprs
    }

defnBinaryStatment :: KoakParser (OpDefn Untyped)
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
    ret_ty <- lift $ optSpacing *> typeName
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
        opdefn_args = [Untyped arg1, Untyped arg2],
        opdefn_ret_ty = ret_ty,
        opdefn_body = Untyped exprs
    }

defnFuncStatment :: KoakParser (FnDefn Untyped)
defnFuncStatment = do
    lift $ optSpacing *> pString "def"
    name <- lift $ optSpacing *> identifier
    args <- lift $ between
        (optSpacing *> pChar '(')
        (optSpacing *> pChar ')')
        (sepBy0 (optSpacing *> pChar ',') argument)
    lift $ optSpacing *> pChar ':'
    ret_ty <- lift $ optSpacing *> typeName
    lift optSpacing
    exprs <- expressions
    lift $ optSpacing *> pChar ';'
    return $ FnDefn {
        fndefn_name = name,
        fndefn_args = Untyped <$> args,
        fndefn_ret_ty = ret_ty,
        fndefn_body = Untyped exprs
    }

defnStatement :: KoakParser (Defn Untyped)
defnStatement =
    (Op . Untyped <$> defnUnaryStatment)
    <|> (Op . Untyped <$> defnBinaryStatment)
    <|> (Fn . Untyped <$> defnFuncStatment)

exprStatement :: KoakParser (Expr Untyped)
exprStatement = do
    lift optSpacing
    expr <- expressions
    lift $ optSpacing *> pChar ';'
    return expr

statement :: KoakParser (Stmt Untyped)
statement =
    (DefnStmt . Untyped <$> defnStatement)
    <|> (ExprStmt . Untyped <$> exprStatement)

statements :: KoakParser (AST Untyped)
statements = do
    lift optSpacing
    stmt <- Untyped <$> statement
    s <- get
    lift $ fallback [stmt] $ do
        stmts <- evalStateT statements s
        return (stmt : stmts)

program :: Parser (AST Untyped)
program = evalStateT allStatements defaultPrecedenceMap
    where
        allStatements :: KoakParser (AST Untyped)
        allStatements = do
            stmts <- statements
            lift $ optSpacing *> pEnd
            return stmts
