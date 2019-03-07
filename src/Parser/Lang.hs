{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Parser.Lang where

import Annotation
import Misc
import Types as Ty
import Parser.Lib
import Control.Applicative
import Control.Monad.State.Lazy

import Data.Char (isSpace)
import Data.List (sortBy)

type Located a = Ann Range a
type LocatedArg = Located Arg
type LocatedExpr = Located (Expr Range)
type LocatedStmt = Located (Stmt Range)
type LocatedAST = AST Range

type Typed a = Ann (Range, Type) a
type TypedArg = Typed Arg
type TypedExpr = Typed (Expr (Range, Type))
type TypedStmt = Typed (Stmt (Range, Type))
type TypedAST = AST (Range, Type)

type Schemed a = Ann (Range, Scheme) a
type SchemedArg = Schemed Arg
type SchemedExpr = Schemed (Expr (Range, Scheme))
type SchemedStmt = Schemed (Stmt (Range, Scheme))
type SchemedAST = AST (Range, Scheme)

data PrecMap = PrecMap {
    bin_prec :: [(String, Assoc Int)],
    un_prec :: [(String, Int)]
} deriving (Show, Eq)

type KoakParser = StateT PrecMap Parser

intoParser :: PrecMap -> KoakParser a -> Parser a
intoParser = flip evalStateT

type AST a = [Ann a (Stmt a)]
data Arg = Arg String Type deriving (Show, Eq)
data DefnType =
    Function
    | Unary Int
    | Binary (Assoc Int)
    deriving (Show, Eq)

data Assoc a =
    LeftAssoc a
    | RightAssoc a
    deriving (Show, Eq, Ord)
precedence :: Assoc a -> a
precedence = \case
    LeftAssoc i -> i
    RightAssoc i -> i

data Stmt a =
    Defn DefnType String [Ann a Arg] Type (Ann a (Expr a))
    | Extern String [Ann a Arg] Type
    | Expr (Ann a (Expr a))
    deriving (Show, Eq)

data Expr a =
    Call (Ann a String) [Ann a (Expr a)]
    | Un (Ann a String) (Ann a (Expr a))
    | Bin (Ann a String) (Ann a (Expr a)) (Ann a (Expr a))
    | Lit Literal
    | Ident String
    | For (Ann a (Expr a)) (Ann a (Expr a)) (Ann a (Expr a)) (Ann a (Expr a))
    | If (Ann a (Expr a)) (Ann a (Expr a)) (Maybe (Ann a (Expr a)))
    | While (Ann a (Expr a)) (Ann a (Expr a))
    | Lambda [Ann a String] (Ann a (Expr a))
    deriving (Show, Eq)

data Literal =
    DoubleLiteral Double
    | IntLiteral Int
    | BooleanLiteral Bool
    | VoidLiteral
    deriving (Show, Eq)

withRangeT :: (Monad (m Parser), MonadTrans m) => m Parser a -> m Parser (a, Range)
withRangeT parser = do
    start <- lift currentPos
    ret <- parser
    end <- lift currentPos
    return (ret, Range { start, end })

defaultPrecedenceMap :: PrecMap
defaultPrecedenceMap = PrecMap {
    bin_prec = [ ( "*", LeftAssoc 50), ( "/", LeftAssoc 50), ( "%", LeftAssoc 50)
               , ( "+", LeftAssoc 40), ( "-", LeftAssoc 40)
               , ( "<", LeftAssoc 30), ( ">", LeftAssoc 30)
               , ("==", LeftAssoc 20), ("!=", LeftAssoc 20)
               , ( "=", RightAssoc 10)
               , ( ":", LeftAssoc 1) ],
    un_prec  = [ ("-", 30), ("!", 30) ]
}

lookupAll :: (k -> Bool) -> [(k, v)] -> [(k, v)]
lookupAll f = filter (f . fst)

binOp :: KoakParser (String, Assoc Int)
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

readDouble :: String -> Double
readDouble = read

lower, upper, digit, symbol, dot :: Parser Char
lower = pRange 'a' 'z'
upper = pRange 'A' 'Z'
digit = pRange '0' '9'
symbol = pAnyOf "!#$%&*=+<.>/?@\\-^|~"
dot = pChar '.'

identifier :: Parser String
identifier = (:) <$> (lower <|> upper) <*> many (lower <|> upper <|> digit)

primitiveType :: Parser Type
primitiveType =
    (const Ty.int <$> pString "integer")
    <|> (const Ty.double <$> pString "double")
    <|> (const Ty.bool <$> pString "bool")
    <|> (const Ty.void <$> pString "void")

functionType :: Parser Type
functionType = do
    args <- between
        (pChar '(')
        (optSpacing *> pChar ')')
        (sepBy0 (optSpacing *> pChar ',') (optSpacing *> typeName))
    _ <- optSpacing *> pString "->"
    ret_ty <- optSpacing *> typeName
    return (args :-> ret_ty)

typeName :: Parser Type
typeName = primitiveType <|> functionType

integer :: Parser Int
integer = do
    digits <- some digit
    return $ readInt digits

decimalConst :: Parser Literal
decimalConst = IntLiteral <$> integer

doubleConst :: Parser Literal
doubleConst =
    let parser1 = do
            intPart <- some digit
            dot' <- dot
            decPart <- fallback "0" $ some digit
            return (intPart <> (dot' : decPart))
        parser2 = do
            dot' <- dot
            decPart <- some digit
            return ('0' : dot' : decPart)
    in fmap (\s -> s |> readDouble |> DoubleLiteral) (parser1 <|> parser2)

booleanLiteral :: Parser Literal
booleanLiteral =
    ((const (BooleanLiteral True) <$> pString "true")
    <|> (const (BooleanLiteral False) <$> pString "false")) <* pNot (lower <|> upper)

literal :: Parser Literal
literal =
    (doubleConst <|> decimalConst <|> booleanLiteral <|> (const VoidLiteral <$> pString "()"))
        ?? "Expected a literal"

spacing, optSpacing :: Parser String
spacing = some (satisfy isSpace <|> (pString "--" *> whileNot (pChar '\n') *> pChar '\n'))
optSpacing = many (satisfy isSpace <|> (pString "--" *> whileNot (pChar '\n') *> pChar '\n'))

expressions :: KoakParser LocatedExpr
expressions =
    forExpression
    <|> ifExpression
    <|> whileExpression
    <|> lambdaExpression
    <|> expression

lambdaExpression :: KoakParser LocatedExpr
lambdaExpression = do
    ((args, body), range) <- withRangeT $ do
        args <- lift (sepBy1 spacing $ do
            (arg, range) <- optSpacing *> withRange identifier
            return $ Ann range arg)
        _ <- lift (optSpacing *> pString "->")
        _ <- lift optSpacing
        body <- expressions
        return (args, body)
    return $ Ann range $ Lambda args body

forExpression :: KoakParser LocatedExpr
forExpression = do
    ((init, cond, oper, body), range) <- withRangeT $ do
        _ <- lift $ optSpacing *> pString "for"
        _ <- lift spacing
        init <- expression
        _ <- lift $ optSpacing *> pChar ','
        _ <- lift optSpacing
        cond <- expression
        _ <- lift $ optSpacing *> pChar ','
        _ <- lift optSpacing
        oper <- expression
        _ <- lift $ optSpacing *> pString "in"
        _ <- lift optSpacing
        body <- expressions
        return (init, cond, oper, body)
    return $ Ann range $ For init cond oper body

ifExpression :: KoakParser LocatedExpr
ifExpression = do
    ((cond, then_body, else_body), range) <- withRangeT $ do
        _ <- lift $ optSpacing *> pString "if"
        _ <- lift spacing
        cond <- expression
        _ <- lift $ spacing *> pString "then"
        _ <- lift spacing
        then_body <- expressions
        precMap <- get
        else_body <- lift $ optional $ do
            _ <- spacing
            _ <- pString "else"
            _ <- spacing
            evalStateT expressions precMap
        return (cond, then_body, else_body)
    return $ Ann range $ If cond then_body else_body

whileExpression :: KoakParser LocatedExpr
whileExpression = do
    ((cond, body), range) <- withRangeT $ do
        _ <- lift $ optSpacing *> pString "while"
        _ <- lift spacing
        cond <- expression
        _ <- lift $ spacing *> pString "do"
        _ <- lift spacing
        body <- expressions
        return (cond, body)
    return $ Ann range $ While cond body

parseBinOp :: (LocatedExpr, Range) -> Int -> KoakParser LocatedExpr
parseBinOp lhs minPrec = do
    s <- get
    let pBinOp = evalStateT binOp s
    let pUnary = evalStateT unary s
    let outer lhs minPrec = fallback (fst lhs) $ do
         _ <- optSpacing
         (_, prec) <- peek pBinOp
         let iprec = precedence prec
         if iprec < minPrec then empty else fallback (fst lhs) $ do
             _ <- optSpacing
             ((op, _), op_range) <- withRange pBinOp
             _ <- optSpacing
             rhs <- withRange pUnary
             let range = Range{ start = start (snd lhs), end = end (snd rhs) }
             let inner rhs' = fallback (fst rhs') $ do
                  _ <- optSpacing
                  (_, prec') <- peek pBinOp
                  let cond = case prec' of
                       LeftAssoc i -> i <= iprec
                       RightAssoc i -> i /= iprec
                  if cond then empty else fallback (fst rhs') $ do
                     let pBinExpr = evalStateT (parseBinOp rhs' $ precedence prec') s
                     _ <- optSpacing
                     rhs'' <- withRange pBinExpr
                     inner rhs''
             lhs' <- fallback (Ann range $ Bin (Ann op_range op) (fst lhs) (fst rhs), range) $ do
                rhs' <- withRange $ inner rhs
                let range' = Range{ start = start (snd lhs), end = end (snd rhs') }
                return (Ann range' $ Bin (Ann op_range op) (fst lhs) (fst rhs'), range')
             outer lhs' minPrec
    lift $ outer lhs minPrec

expression :: KoakParser LocatedExpr
expression = do
    _ <- lift optSpacing
    lhs <- withRangeT unary
    parseBinOp lhs 0

unary :: KoakParser LocatedExpr
unary = do
    _ <- lift optSpacing
    let p1 = do
         ((op, op_range, rhs), range) <- withRangeT $ do
             ((op, _), op_range) <- withRangeT unOp
             _ <- lift optSpacing
             rhs <- unary
             return (op, op_range, rhs)
         return $ Ann range $ Un (Ann op_range op) rhs
    p1 <|> postfix

postfix :: KoakParser LocatedExpr
postfix = do
    _ <- lift optSpacing
    let ident = do
         (ident', range) <- lift $ withRange identifier
         return $ Ann range $ Ident ident'
    let lit = do
         (lit', range) <- lift $ withRange literal
         return $ Ann range $ Lit lit'
    let paren = do
         _ <- lift $ pChar '('
         _ <- lift optSpacing
         expr <- expressions
         _ <- lift $ optSpacing *> pChar ')'
         return expr
    let primary = lit <|> ident <|> paren
    let callExpr = do
         ((name, name_range, args), range) <- withRangeT $ do
            (name, name_range) <- lift $ withRange identifier
            _ <- lift optSpacing
            args <- do
               _ <- lift $ pChar '('
               precs <- get
               let pExpr = evalStateT expression precs
               args <- lift $ sepBy0 (optSpacing *> pChar ',') (optSpacing *> pExpr)
               _ <- lift optSpacing
               _ <- lift $ pChar ')'
               return args
            return (name, name_range, args)
         return $ Ann range $ Call (Ann name_range name) args
    callExpr <|> primary

argument :: Parser LocatedArg
argument = do
    ((name, ty), range) <- withRange $ do
        name <- optSpacing *> identifier
        _ <- optSpacing *> pChar ':'
        ty <- optSpacing *> typeName
        return (name, ty)
    return $ Ann range $ Arg name ty

defnUnaryStatment :: KoakParser LocatedStmt
defnUnaryStatment = do
    ((name, prec, rhs, ret_ty, body), range) <- withRangeT $ do
        _ <- lift $ optSpacing *> pString "def"
        _ <- lift $ spacing *> pString "unary"
        name <- lift $ optSpacing *> some symbol
        prec <- lift $ fallback 25 (optSpacing *> integer)
        rhs <- lift $ between
            (optSpacing *> pChar '(')
            (optSpacing *> pChar ')')
            argument
        _ <- lift $ optSpacing *> pChar ':'
        ret_ty <- lift $ optSpacing *> typeName
        _ <- lift optSpacing
        body <- expressions
        _ <- lift $ optSpacing *> pChar ';'
        return (name, prec, rhs, ret_ty, body)
    s <- get
    put $ PrecMap {
        un_prec = (name, prec) : un_prec s,
        bin_prec = bin_prec s
    }
    return $ Ann range $ Defn (Unary prec) name [rhs] ret_ty body

defnBinaryStatment :: KoakParser LocatedStmt
defnBinaryStatment = do
    ((name, prec, lhs, rhs, ret_ty, body), range) <- withRangeT $ do
        _ <- lift $ optSpacing *> pString "def"
        _ <- lift $ spacing *> pString "binary"
        name <- lift $ optSpacing *> some symbol
        prec <- lift $ fallback (LeftAssoc 25) $ do
            iprec <- optSpacing *> integer
            fallback (LeftAssoc iprec) $
                let left = const (LeftAssoc iprec) <$> pString "left"
                    right = const (RightAssoc iprec) <$> pString "right"
                in optSpacing *> (left <|> right)
        (lhs, rhs) <- lift $ between
            (optSpacing *> pChar '(')
            (optSpacing *> pChar ')')
            (both argument (spacing *> argument))
        _ <- lift $ optSpacing *> pChar ':'
        ret_ty <- lift $ optSpacing *> typeName
        _ <- lift optSpacing
        body <- expressions
        _ <- lift $ optSpacing *> pChar ';'
        return (name, prec, lhs, rhs, ret_ty, body)
    s <- get
    put $ PrecMap {
        un_prec = un_prec s,
        bin_prec = (name, prec) : bin_prec s
    }
    return $ Ann range $ Defn (Binary prec) name [lhs, rhs] ret_ty body

defnFuncStatment :: KoakParser LocatedStmt
defnFuncStatment = do
    ((name, args, ret_ty, body), range) <- withRangeT $ do
        _ <- lift $ optSpacing *> pString "def"
        name <- lift $ spacing *> identifier
        args <- lift $ between
            (optSpacing *> pChar '(')
            (optSpacing *> pChar ')')
            (sepBy0 spacing argument)
        _ <- lift $ optSpacing *> pChar ':'
        ret_ty <- lift $ optSpacing *> typeName
        _ <- lift optSpacing
        body <- expressions
        _ <- lift $ optSpacing *> pChar ';'
        return (name, args, ret_ty, body)
    return $ Ann range $ Defn Function name args ret_ty body

defnStatement :: KoakParser LocatedStmt
defnStatement =
    defnUnaryStatment
    <|> defnBinaryStatment
    <|> defnFuncStatment

exprStatement :: KoakParser LocatedStmt
exprStatement = do
    (expr, range) <- withRangeT $ do
        _ <- lift optSpacing
        expr <- expressions
        _ <- lift $ optSpacing *> pChar ';'
        return expr
    return $ Ann range $ Expr expr

externStatement :: KoakParser LocatedStmt
externStatement = do
    ((name, args, ret_ty), range) <- withRangeT $ do
        _ <- lift $ optSpacing *> pString "extern"
        name <- lift $ spacing *> identifier
        args <- lift $ between
            (optSpacing *> pChar '(')
            (optSpacing *> pChar ')')
            (sepBy0 (optSpacing *> pChar ',') argument)
        _ <- lift $ optSpacing *> pChar ':'
        ret_ty <- lift $ optSpacing *> typeName
        _ <- lift $ optSpacing *> pChar ';'
        return (name, args, ret_ty)
    return $ Ann range $ Extern name args ret_ty

statement :: KoakParser LocatedStmt
statement =
    defnStatement
    <|> exprStatement
    <|> externStatement

statements :: KoakParser (AST Range)
statements = do
    _ <- lift optSpacing
    stmt <- statement
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
            _ <- lift $ optSpacing *> pEnd
            return stmts
