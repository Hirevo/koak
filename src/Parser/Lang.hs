{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Parser.Lang where

import Annotation
import Misc
import Parser.Lib
import Control.Applicative
import Control.Monad.State.Lazy
import System.IO.Unsafe

import Data.Char (isSpace)
import Data.List (sortBy)

data PrecMap = PrecMap {
    bin_prec :: [(String, Int)],
    un_prec :: [(String, Int)]
} deriving (Show, Eq)

type KoakParser = StateT PrecMap Parser

intoParser :: PrecMap -> KoakParser a -> Parser a
intoParser = flip evalStateT

type AST a = [Ann a (Stmt a)]
data Stmt a =
    DefnStmt (Ann a (Defn a))
    | ExprStmt (Ann a (Expr a))
    deriving (Show, Eq)
isDefnStmt, isExprStmt :: Stmt a -> Bool
isDefnStmt (DefnStmt _) = True
isDefnStmt _ = False
isExprStmt (ExprStmt _) = True
isExprStmt _ = False

data Type =
    IntType
    | FloatType
    | VoidType
    deriving (Show, Eq)
isIntType, isDoubleType, isVoidType :: Type -> Bool
isIntType = (== IntType)
isDoubleType = (== FloatType)
isVoidType = (== VoidType)

data Arg = Arg {
    arg_name :: String,
    arg_type :: Type
} deriving (Show, Eq)
data Arity =
    Unary
    | Binary
    deriving (Show, Eq)
isUnary, isBinary :: Arity -> Bool
isUnary = (== Unary)
isBinary = (== Binary)

data Defn a =
    Op (Ann a (OpDefn a))
    | Fn (Ann a (FnDefn a))
    deriving (Show, Eq)
isOpDefn, isFnDefn :: Defn a -> Bool
isOpDefn (Op _) = True
isOpDefn _ = False
isFnDefn (Fn _) = True
isFnDefn _ = False

data OpDefn a = OpDefn {
    opdefn_op :: String,
    opdefn_prec :: Int,
    opdefn_arity :: Arity,
    opdefn_args :: [Ann a Arg],
    opdefn_ret_ty :: Type,
    opdefn_body :: Ann a (Expr a)
} deriving (Show, Eq)
data FnDefn a = FnDefn {
    fndefn_name :: String,
    fndefn_args :: [Ann a Arg],
    fndefn_ret_ty :: Type,
    fndefn_body :: Ann a (Expr a)
} deriving (Show, Eq)
data Expr a =
    Call (Ann a (CallExpr a))
    | Bin (Ann a (BinExpr a))
    | Un (Ann a (UnExpr a))
    | Lit (Ann a Literal)
    | Ident (Ann a String)
    | For (Ann a (ForExpr a))
    | If (Ann a (IfExpr a))
    | While (Ann a (WhileExpr a))
    deriving (Show, Eq)
isCallExpr, isBinExpr, isUnExpr, isLitExpr :: Expr a -> Bool
isIdentExpr, isForExpr, isIfExpr, isWhileExpr :: Expr a -> Bool
isCallExpr (Call _) = True
isCallExpr _ = False
isBinExpr (Bin _) = True
isBinExpr _ = False
isUnExpr (Un _) = True
isUnExpr _ = False
isLitExpr (Lit _) = True
isLitExpr _ = False
isIdentExpr (Ident _) = True
isIdentExpr _ = False
isForExpr (For _) = True
isForExpr _ = False
isIfExpr (If _) = True
isIfExpr _ = False
isWhileExpr (While _) = True
isWhileExpr _ = False

data ForExpr a = ForExpr {
    for_init :: Ann a (Expr a),
    for_cond :: Ann a (Expr a),
    for_oper :: Ann a (Expr a),
    for_body :: Ann a (Expr a)
} deriving (Show, Eq)
data IfExpr a = IfExpr {
    if_cond :: Ann a (Expr a),
    if_then :: Ann a (Expr a),
    if_else :: Maybe (Ann a (Expr a))
} deriving (Show, Eq)
data WhileExpr a = WhileExpr {
    while_cond :: Ann a (Expr a),
    while_body :: Ann a (Expr a)
} deriving (Show, Eq)
data Literal =
    FloatLiteral Float
    | IntLiteral Int
    | VoidLiteral
    deriving (Show, Eq)
isIntLiteral, isDoubleLiteral, isVoidLiteral :: Literal -> Bool
isIntLiteral (IntLiteral _) = True
isIntLiteral _ = False
isDoubleLiteral (FloatLiteral _) = True
isDoubleLiteral _ = False
isVoidLiteral VoidLiteral = True
isVoidLiteral _ = False

data CallExpr a = CallExpr {
    call_ident :: String,
    call_args :: [Ann a (Expr a)]
} deriving (Show, Eq)
data BinExpr a = BinExpr {
    bin_op :: String,
    bin_lhs :: Ann a (Expr a),
    bin_rhs :: Ann a (Expr a)
} deriving (Show, Eq)
data UnExpr a = UnExpr {
    un_op :: String,
    un_rhs :: Ann a (Expr a)
} deriving (Show, Eq)

withRangeT :: (Monad (m Parser), MonadTrans m) => m Parser a -> m Parser (a, Range)
withRangeT parser = do
    start <- lift currentPos
    ret <- parser
    end <- lift currentPos
    return (ret, Range { start, end })

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

lower, upper, digit, symbol, dot :: Parser Char
lower = pRange 'a' 'z'
upper = pRange 'A' 'Z'
digit = pRange '0' '9'
symbol = pAnyOf "!#$%&*=+<.>/?@\\-^|~"
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
    (doubleConst
    <|> decimalConst
    <|> (const VoidLiteral <$> pString "()")) ?? "Expected a literal"

spacing, optSpacing :: Parser String
spacing = some (satisfy isSpace <|> (pString "--" *> whileNot (pChar '\n') *> pChar '\n'))
optSpacing = many (satisfy isSpace <|> (pString "--" *> whileNot (pChar '\n') *> pChar '\n'))

expressions :: KoakParser (Expr Range)
expressions =
    (For . uncurry (flip Ann) <$> withRangeT forExpression)
    <|> (If . uncurry (flip Ann) <$> withRangeT ifExpr)
    <|> (While . uncurry (flip Ann) <$> withRangeT whileExpr)
    <|> expression

forExpression :: KoakParser (ForExpr Range)
forExpression = do
    lift $ optSpacing *> pString "for"
    lift spacing
    (init, init_range) <- withRangeT expression
    lift $ optSpacing *> pChar ','
    lift optSpacing
    (cond, cond_range) <- withRangeT expression
    lift $ optSpacing *> pChar ','
    lift optSpacing
    (oper, oper_range) <- withRangeT expression
    lift $ optSpacing *> pString "in"
    lift optSpacing
    (body, body_range) <- withRangeT expressions
    return $ ForExpr {
        for_init = Ann init_range init,
        for_cond = Ann cond_range cond,
        for_oper = Ann oper_range oper,
        for_body = Ann body_range body
    }

ifExpr :: KoakParser (IfExpr Range)
ifExpr = do
    lift $ optSpacing *> pString "if"
    lift spacing
    (if_expr, if_range) <- withRangeT expression
    lift $ spacing *> pString "then"
    lift spacing
    (then_body, then_range) <- withRangeT expressions
    precMap <- get
    else_body <- lift $ optional $ withRange $ do
        spacing
        pString "else"
        spacing
        evalStateT expressions precMap
    return $ IfExpr {
        if_cond = Ann if_range if_expr,
        if_then = Ann then_range then_body,
        if_else = uncurry (flip Ann) <$> else_body
    }

whileExpr :: KoakParser (WhileExpr Range)
whileExpr = do
    lift $ optSpacing *> pString "while"
    lift spacing
    (while_expr, while_range) <- withRangeT expression
    lift $ spacing *> pString "do"
    lift spacing
    (body, body_range) <- withRangeT expressions
    return $ WhileExpr {
        while_cond = Ann while_range while_expr,
        while_body = Ann body_range body
    }

parseBinOp :: (Expr Range, Range) -> Int -> KoakParser (Expr Range)
parseBinOp lhs minPrec = do
    s <- get
    let pBinOp = evalStateT binOp s
    let pUnary = evalStateT unary s
    let outer lhs minPrec = fallback (fst lhs) $ do
         optSpacing
         (_, prec) <- peek pBinOp
         if prec < minPrec then empty else fallback (fst lhs) $ do
             optSpacing
             (op, prec) <- pBinOp
             optSpacing
             rhs <- withRange pUnary
             let range = Range{ start = start (snd lhs), end = end (snd rhs) }
             let expr = Bin $ Ann range $ BinExpr {
                 bin_op = op,
                 bin_lhs = Ann (snd lhs) (fst lhs),
                 bin_rhs = Ann (snd rhs) (fst rhs)
             }
             let inner rhs' = fallback (fst rhs') $ do
                  optSpacing
                  (_, prec') <- peek pBinOp
                  if prec' <= prec then empty else fallback (fst rhs') $ do
                     let pBinExpr = evalStateT (parseBinOp rhs' prec') s
                     optSpacing
                     rhs <- withRange pBinExpr
                     inner rhs
             lhs' <- fallback (expr, range) $ do
                rhs' <- withRange $ inner rhs
                let range' = Range{ start = start (snd lhs), end = end (snd rhs') }
                return (Bin $ Ann range' $ BinExpr {
                    bin_op = op,
                    bin_lhs = Ann (snd lhs) (fst lhs),
                    bin_rhs = Ann (snd rhs') (fst rhs')
                }, range')
             outer lhs' minPrec
    lift $ outer lhs minPrec

expression :: KoakParser (Expr Range)
expression = do
    precMap <- get
    lift optSpacing
    lhs <- withRangeT unary
    parseBinOp lhs 0

unary :: KoakParser (Expr Range)
unary = do
    precMap <- get
    lift optSpacing
    let p1 = do
         (expr, range) <- withRangeT $ do
             operator <- lift $ choice $ map (pString . fst) $ un_prec precMap
             lift optSpacing
             (operand, operand_range) <- withRangeT unary
             return UnExpr {
                 un_op = operator,
                 un_rhs = Ann operand_range operand
             }
         return $ Un $ Ann range expr
    p1 <|> postfix

postfix :: KoakParser (Expr Range)
postfix = do
    precMap <- get
    lift optSpacing
    let ident = lift $ Ident . uncurry (flip Ann) <$> withRange identifier
    let lit = lift $ Lit . uncurry (flip Ann) <$> withRange literal
    let paren = do
         lift $ pChar '('
         lift optSpacing
         expr <- expressions
         lift $ optSpacing *> pChar ')'
         return expr
    let primary = ident <|> lit <|> paren
    let callExpr = do
         start <- lift currentPos
         name <- lift identifier
         lift optSpacing
         args <- do
            lift $ pChar '('
            precMap <- get
            let pExpr = evalStateT expression precMap
            args <- lift $ sepBy0 (optSpacing *> pChar ',') (uncurry (flip Ann) <$> withRange (optSpacing *> pExpr))
            lift optSpacing
            lift $ pChar ')'
            return args
         end <- lift currentPos
         return $ Call $ Ann Range{ start, end } $ CallExpr {
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

defnUnaryStatment :: KoakParser (OpDefn Range)
defnUnaryStatment = do
    lift $ optSpacing *> pString "def"
    lift $ spacing *> pString "unary"
    name <- lift $ optSpacing *> some symbol
    prec <- lift $ optSpacing *> integer
    (arg, arg_range) <- lift $ between
        (optSpacing *> pChar '(')
        (optSpacing *> pChar ')')
        (withRange argument)
    lift $ optSpacing *> pChar ':'
    ret_ty <- lift $ optSpacing *> typeName
    lift optSpacing
    (exprs, exprs_range) <- withRangeT expressions
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
        opdefn_args = [Ann arg_range arg],
        opdefn_ret_ty = ret_ty,
        opdefn_body = Ann exprs_range exprs
    }

defnBinaryStatment :: KoakParser (OpDefn Range)
defnBinaryStatment = do
    lift $ optSpacing *> pString "def"
    lift $ spacing *> pString "binary"
    name <- lift $ optSpacing *> some symbol
    prec <- lift $ optSpacing *> integer
    ((arg1, arg1_range), (arg2, arg2_range)) <- lift $ between
        (optSpacing *> pChar '(')
        (optSpacing *> pChar ')')
        (both (withRange argument) (withRange (optSpacing *> pChar ',' *> argument)))
    lift $ optSpacing *> pChar ':'
    ret_ty <- lift $ optSpacing *> typeName
    lift optSpacing
    (exprs, exprs_range) <- withRangeT expressions
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
        opdefn_args = [Ann arg1_range arg1, Ann arg2_range arg2],
        opdefn_ret_ty = ret_ty,
        opdefn_body = Ann exprs_range exprs
    }

defnFuncStatment :: KoakParser (FnDefn Range)
defnFuncStatment = do
    lift $ optSpacing *> pString "def"
    name <- lift $ optSpacing *> identifier
    args <- lift $ between
        (optSpacing *> pChar '(')
        (optSpacing *> pChar ')')
        (sepBy0 (optSpacing *> pChar ',') (withRange argument))
    lift $ optSpacing *> pChar ':'
    ret_ty <- lift $ optSpacing *> typeName
    lift optSpacing
    (exprs, exprs_range) <- withRangeT expressions
    lift $ optSpacing *> pChar ';'
    return $ FnDefn {
        fndefn_name = name,
        fndefn_args = uncurry (flip Ann) <$> args,
        fndefn_ret_ty = ret_ty,
        fndefn_body = Ann exprs_range exprs
    }

defnStatement :: KoakParser (Defn Range)
defnStatement =
    (Op . uncurry (flip Ann) <$> withRangeT defnUnaryStatment)
    <|> (Op . uncurry (flip Ann) <$> withRangeT defnBinaryStatment)
    <|> (Fn . uncurry (flip Ann) <$> withRangeT defnFuncStatment)

exprStatement :: KoakParser (Expr Range)
exprStatement = do
    lift optSpacing
    expr <- expressions
    lift $ optSpacing *> pChar ';'
    return expr

statement :: KoakParser (Stmt Range)
statement =
    (DefnStmt . uncurry (flip Ann) <$> withRangeT defnStatement)
    <|> (ExprStmt . uncurry (flip Ann) <$> withRangeT exprStatement)

statements :: KoakParser (AST Range)
statements = do
    lift optSpacing
    stmt <- uncurry (flip Ann) <$> withRangeT statement
    s <- get
    lift $ fallback [stmt] $ do
        stmts <- evalStateT statements s
        return (stmt : stmts)

program :: Parser (AST Range)
program = evalStateT allStatements defaultPrecedenceMap
    where
        allStatements :: KoakParser (AST Range)
        allStatements = do
            stmts <- statements
            lift $ optSpacing *> pEnd
            return stmts
