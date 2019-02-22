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
import System.IO.Unsafe

import Data.Char (isSpace)
import Data.List (sortBy)

import qualified Data.Map as Map

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
    (const Ty.int <$> pString "int")
    <|> (const Ty.double <$> pString "double")
    <|> (const Ty.bool <$> pString "bool")
    <|> (const Ty.void <$> pString "void")

functionType :: Parser Type
functionType = do
    args <- between
        (pChar '(')
        (optSpacing *> pChar ')')
        (sepBy0 (optSpacing *> pChar ',') (optSpacing *> typeName))
    optSpacing *> pString "->"
    ret_ty <- optSpacing *> typeName
    return $ TFun Map.empty args ret_ty

typeName :: Parser Type
typeName = primitiveType -- <|> functionType

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
            dot <- dot
            decPart <- fallback "0" $ some digit
            return (intPart <> (dot : decPart))
        parser2 = do
            dot <- dot
            decPart <- some digit
            return ('0' : dot : decPart)
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

expressions :: KoakParser (Ann Range (Expr Range))
expressions =
    forExpression
    <|> ifExpr
    <|> whileExpr
    <|> expression

forExpression :: KoakParser (Ann Range (Expr Range))
forExpression = do
    ((init, cond, oper, body), range) <- withRangeT $ do
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
        return (init, cond, oper, body)
    return $ Ann range $ For init cond oper body

ifExpr :: KoakParser (Ann Range (Expr Range))
ifExpr = do
    ((cond, then_body, else_body), range) <- withRangeT $ do
        lift $ optSpacing *> pString "if"
        lift spacing
        cond <- expression
        lift $ spacing *> pString "then"
        lift spacing
        then_body <- expressions
        precMap <- get
        else_body <- lift $ optional $ do
            spacing
            pString "else"
            spacing
            evalStateT expressions precMap
        return (cond, then_body, else_body)
    return $ Ann range $ If cond then_body else_body

whileExpr :: KoakParser (Ann Range (Expr Range))
whileExpr = do
    ((cond, body), range) <- withRangeT $ do
        lift $ optSpacing *> pString "while"
        lift spacing
        cond <- expression
        lift $ spacing *> pString "do"
        lift spacing
        body <- expressions
        return (cond, body)
    return $ Ann range $ While cond body

parseBinOp :: (Ann Range (Expr Range), Range) -> Int -> KoakParser (Ann Range (Expr Range))
parseBinOp lhs minPrec = do
    s <- get
    let pBinOp = evalStateT binOp s
    let pUnary = evalStateT unary s
    let outer lhs minPrec = fallback (fst lhs) $ do
         optSpacing
         (_, prec) <- peek pBinOp
         let iprec = precedence prec
         if iprec < minPrec then empty else fallback (fst lhs) $ do
             optSpacing
             ((op, prec), op_range) <- withRange pBinOp
             optSpacing
             rhs <- withRange pUnary
             let range = Range{ start = start (snd lhs), end = end (snd rhs) }
             let inner rhs' = fallback (fst rhs') $ do
                  optSpacing
                  (_, prec') <- peek pBinOp
                  let cond = case prec' of
                       LeftAssoc i -> i <= iprec
                       RightAssoc i -> i /= iprec
                  if cond then empty else fallback (fst rhs') $ do
                     let pBinExpr = evalStateT (parseBinOp rhs' $ precedence prec') s
                     optSpacing
                     rhs <- withRange pBinExpr
                     inner rhs
             lhs' <- fallback (Ann range $ Bin (Ann op_range op) (fst lhs) (fst rhs), range) $ do
                rhs' <- withRange $ inner rhs
                let range' = Range{ start = start (snd lhs), end = end (snd rhs') }
                return (Ann range' $ Bin (Ann op_range op) (fst lhs) (fst rhs'), range')
             outer lhs' minPrec
    lift $ outer lhs minPrec

expression :: KoakParser (Ann Range (Expr Range))
expression = do
    precMap <- get
    lift optSpacing
    lhs <- withRangeT unary
    parseBinOp lhs 0

unary :: KoakParser (Ann Range (Expr Range))
unary = do
    precMap <- get
    lift optSpacing
    let p1 = do
         ((op, op_range, rhs), range) <- withRangeT $ do
             ((op, _), op_range) <- withRangeT unOp
             lift optSpacing
             rhs <- unary
             return (op, op_range, rhs)
         return $ Ann range $ Un (Ann op_range op) rhs
    p1 <|> postfix

postfix :: KoakParser (Ann Range (Expr Range))
postfix = do
    precMap <- get
    lift optSpacing
    let ident = do
         (ident', range) <- lift $ withRange identifier
         return $ Ann range $ Ident ident'
    let lit = do
         (lit', range) <- lift $ withRange literal
         return $ Ann range $ Lit lit'
    let paren = do
         lift $ pChar '('
         lift optSpacing
         expr <- expressions
         lift $ optSpacing *> pChar ')'
         return expr
    let primary = lit <|> ident <|> paren
    let callExpr = do
         ((name, name_range, args), range) <- withRangeT $ do
            (name, name_range) <- lift $ withRange identifier
            lift optSpacing
            args <- do
               lift $ pChar '('
               precMap <- get
               let pExpr = evalStateT expression precMap
               args <- lift $ sepBy0 (optSpacing *> pChar ',') (optSpacing *> pExpr)
               lift optSpacing
               lift $ pChar ')'
               return args
            return (name, name_range, args)
         return $ Ann range $ Call (Ann name_range name) args
    callExpr <|> primary

argument :: Parser (Ann Range Arg)
argument = do
    ((name, ty), range) <- withRange $ do
        name <- optSpacing *> identifier
        optSpacing *> pChar ':'
        ty <- optSpacing *> typeName
        return (name, ty)
    return $ Ann range $ Arg name ty

defnUnaryStatment :: KoakParser (Ann Range (Stmt Range))
defnUnaryStatment = do
    ((name, prec, rhs, ret_ty, body), range) <- withRangeT $ do
        lift $ optSpacing *> pString "def"
        lift $ spacing *> pString "unary"
        name <- lift $ optSpacing *> some symbol
        prec <- lift $ fallback 25 (optSpacing *> integer)
        rhs <- lift $ between
            (optSpacing *> pChar '(')
            (optSpacing *> pChar ')')
            argument
        lift $ optSpacing *> pChar ':'
        ret_ty <- lift $ optSpacing *> typeName
        lift optSpacing
        body <- expressions
        lift $ optSpacing *> pChar ';'
        return (name, prec, rhs, ret_ty, body)
    s <- get
    put $ PrecMap {
        un_prec = (name, prec) : un_prec s,
        bin_prec = bin_prec s
    }
    return $ Ann range $ Defn (Unary prec) name [rhs] ret_ty body

defnBinaryStatment :: KoakParser (Ann Range (Stmt Range))
defnBinaryStatment = do
    ((name, prec, lhs, rhs, ret_ty, body), range) <- withRangeT $ do
        lift $ optSpacing *> pString "def"
        lift $ spacing *> pString "binary"
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
        lift $ optSpacing *> pChar ':'
        ret_ty <- lift $ optSpacing *> typeName
        lift optSpacing
        body <- expressions
        lift $ optSpacing *> pChar ';'
        return (name, prec, lhs, rhs, ret_ty, body)
    s <- get
    put $ PrecMap {
        un_prec = un_prec s,
        bin_prec = (name, prec) : bin_prec s
    }
    return $ Ann range $ Defn (Binary prec) name [lhs, rhs] ret_ty body

defnFuncStatment :: KoakParser (Ann Range (Stmt Range))
defnFuncStatment = do
    ((name, args, ret_ty, body), range) <- withRangeT $ do
        lift $ optSpacing *> pString "def"
        name <- lift $ spacing *> identifier
        args <- lift $ between
            (optSpacing *> pChar '(')
            (optSpacing *> pChar ')')
            (sepBy0 spacing argument)
        lift $ optSpacing *> pChar ':'
        ret_ty <- lift $ optSpacing *> typeName
        lift optSpacing
        body <- expressions
        lift $ optSpacing *> pChar ';'
        return (name, args, ret_ty, body)
    return $ Ann range $ Defn Function name args ret_ty body

defnStatement :: KoakParser (Ann Range (Stmt Range))
defnStatement =
    defnUnaryStatment
    <|> defnBinaryStatment
    <|> defnFuncStatment

exprStatement :: KoakParser (Ann Range (Stmt Range))
exprStatement = do
    (expr, range) <- withRangeT $ do
        lift optSpacing
        expr <- expressions
        lift $ optSpacing *> pChar ';'
        return expr
    return $ Ann range $ Expr expr

externStatement :: KoakParser (Ann Range (Stmt Range))
externStatement = do
    ((name, args, ret_ty), range) <- withRangeT $ do
        lift $ optSpacing *> pString "extern"
        name <- lift $ spacing *> identifier
        args <- lift $ between
            (optSpacing *> pChar '(')
            (optSpacing *> pChar ')')
            (sepBy0 (optSpacing *> pChar ',') argument)
        lift $ optSpacing *> pChar ':'
        ret_ty <- lift $ optSpacing *> typeName
        lift $ optSpacing *> pChar ';'
        return (name, args, ret_ty)
    return $ Ann range $ Extern name args ret_ty

statement :: KoakParser (Ann Range (Stmt Range))
statement =
    defnStatement
    <|> exprStatement
    <|> externStatement

statements :: KoakParser (AST Range)
statements = do
    lift optSpacing
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
            lift $ optSpacing *> pEnd
            return stmts
