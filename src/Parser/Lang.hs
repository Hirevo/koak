{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    bin_prec :: [(String, Int)],
    un_prec :: [(String, Int)]
} deriving (Show, Eq)

type KoakParser = StateT PrecMap Parser

intoParser :: PrecMap -> KoakParser a -> Parser a
intoParser = flip evalStateT

type AST a = [Stmt a]
data Arg a = Arg a String Type deriving (Show, Eq)
getArgAnn :: Arg a -> a
getArgAnn (Arg a _ _) = a
data DefnType = Function | Unary | Binary deriving (Show, Eq)
data Stmt a =
    Defn a DefnType String [Arg a] Type (Expr a)
    | Extern a String [Arg a] Type
    | Expr a (Expr a)
    deriving (Show, Eq)
isDefn, isExpr, isExtern :: Stmt a -> Bool
isDefn Defn{} = True
isDefnStmt _ = False
isExpr Expr{} = True
isExpr _ = False
isExtern Extern{} = True
isExtern _ = False
getStmtAnn :: Stmt a -> a
getStmtAnn (Defn a _ _ _ _ _) = a
getStmtAnn (Extern a _ _ _) = a
getStmtAnn (Expr a _) = a
setStmtAnn :: a -> Stmt a -> Stmt a
setStmtAnn n (Defn _ a b c d e) = Defn n a b c d e
setStmtAnn n (Extern _ a b c) = Extern n a b c
setStmtAnn n (Expr _ a) = Expr n a

data Expr a =
    Call a (Ann a String) [Expr a]
    | Un a (Ann a String) (Expr a)
    | Bin a (Ann a String) (Expr a) (Expr a)
    | Lit a Literal
    | Ident a String
    | For a (Expr a) (Expr a) (Expr a) (Expr a)
    | If a (Expr a) (Expr a) (Maybe (Expr a))
    | While a (Expr a) (Expr a)
    deriving (Show, Eq)
isCallExpr, isBinExpr, isUnExpr, isLitExpr :: Expr a -> Bool
isIdentExpr, isForExpr, isIfExpr, isWhileExpr :: Expr a -> Bool
isCallExpr Call{} = True
isCallExpr _ = False
isBinExpr Bin{} = True
isBinExpr _ = False
isUnExpr Un{} = True
isUnExpr _ = False
isLitExpr Lit{} = True
isLitExpr _ = False
isIdentExpr Ident{} = True
isIdentExpr _ = False
isForExpr For{} = True
isForExpr _ = False
isIfExpr If{} = True
isIfExpr _ = False
isWhileExpr While{} = True
isWhileExpr _ = False
getExprAnn :: Expr a -> a
getExprAnn (Call a _ _) = a
getExprAnn (Un a _ _) = a
getExprAnn (Bin a _ _ _) = a
getExprAnn (Lit a _) = a
getExprAnn (Ident a _) = a
getExprAnn (For a _ _ _ _) = a
getExprAnn (If a _ _ _) = a
getExprAnn (While a _ _) = a
setExprAnn :: a -> Expr a -> Expr a
setExprAnn n (Call _ a b) = Call n a b
setExprAnn n (Un _ a b) = Un n a b
setExprAnn n (Bin _ a b c) = Bin n a b c
setExprAnn n (Lit _ a) = Lit n a
setExprAnn n (Ident _ a) = Ident n a
setExprAnn n (For _ a b c d) = For n a b c d
setExprAnn n (If _ a b c) = If n a b c
setExprAnn n (While _ a b) = While n a b

data Literal =
    DoubleLiteral Double
    | IntLiteral Int
    | BooleanLiteral Bool
    | VoidLiteral
    deriving (Show, Eq)
isIntLiteral, isDoubleLiteral, isBooleanLiteral, isVoidLiteral :: Literal -> Bool
isIntLiteral (IntLiteral _) = True
isIntLiteral _ = False
isDoubleLiteral (DoubleLiteral _) = True
isDoubleLiteral _ = False
isBooleanLiteral (BooleanLiteral _) = True
isBooleanLiteral _ = False
isVoidLiteral VoidLiteral = True
isVoidLiteral _ = False

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
    let p1 = do
            intPart <- some digit
            dot <- dot
            decPart <- fallback "0" $ some digit
            return (intPart ++ (dot : decPart))
        p2 = do
            dot <- dot
            decPart <- some digit
            return ('0' : dot : decPart)
    in fmap (\s -> s |> readDouble |> DoubleLiteral) (p1 <|> p2)

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

expressions :: KoakParser (Expr Range)
expressions =
    forExpression
    <|> ifExpr
    <|> whileExpr
    <|> expression

forExpression :: KoakParser (Expr Range)
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
    return $ For range init cond oper body

ifExpr :: KoakParser (Expr Range)
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
    return $ If range cond then_body else_body

whileExpr :: KoakParser (Expr Range)
whileExpr = do
    ((cond, body), range) <- withRangeT $ do
        lift $ optSpacing *> pString "while"
        lift spacing
        cond <- expression
        lift $ spacing *> pString "do"
        lift spacing
        body <- expressions
        return (cond, body)
    return $ While range cond body

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
             ((op, prec), op_range) <- withRange pBinOp
             optSpacing
             rhs <- withRange pUnary
             let range = Range{ start = start (snd lhs), end = end (snd rhs) }
             let inner rhs' = fallback (fst rhs') $ do
                  optSpacing
                  (_, prec') <- peek pBinOp
                  if prec' <= prec then empty else fallback (fst rhs') $ do
                     let pBinExpr = evalStateT (parseBinOp rhs' prec') s
                     optSpacing
                     rhs <- withRange pBinExpr
                     inner rhs
             lhs' <- fallback (Bin range (Ann op_range op) (fst lhs) (fst rhs), range) $ do
                rhs' <- withRange $ inner rhs
                let range' = Range{ start = start (snd lhs), end = end (snd rhs') }
                return (Bin range' (Ann op_range op) (fst lhs) (fst rhs'), range')
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
         ((op, op_range, rhs), range) <- withRangeT $ do
             ((op, _), op_range) <- withRangeT unOp
             lift optSpacing
             rhs <- unary
             return (op, op_range, rhs)
         return $ Un range (Ann op_range op) rhs
    p1 <|> postfix

postfix :: KoakParser (Expr Range)
postfix = do
    precMap <- get
    lift optSpacing
    let ident = do
         (ident', range) <- lift $ withRange identifier
         return $ Ident range ident'
    let lit = do
         (lit', range) <- lift $ withRange literal
         return $ Lit range lit'
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
         return $ Call range (Ann name_range name) args
    callExpr <|> primary

argument :: Parser (Arg Range)
argument = do
    ((name, ty), range) <- withRange $ do
        name <- optSpacing *> identifier
        optSpacing *> pChar ':'
        ty <- optSpacing *> typeName
        return (name, ty)
    return $ Arg range name ty

defnUnaryStatment :: KoakParser (Stmt Range)
defnUnaryStatment = do
    ((name, prec, rhs, ret_ty, body), range) <- withRangeT $ do
        lift $ optSpacing *> pString "def"
        lift $ spacing *> pString "unary"
        name <- lift $ optSpacing *> some symbol
        prec <- lift $ optSpacing *> integer
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
    return $ Defn range Unary name [rhs] ret_ty body

defnBinaryStatment :: KoakParser (Stmt Range)
defnBinaryStatment = do
    ((name, prec, lhs, rhs, ret_ty, body), range) <- withRangeT $ do
        lift $ optSpacing *> pString "def"
        lift $ spacing *> pString "binary"
        name <- lift $ optSpacing *> some symbol
        prec <- lift $ optSpacing *> integer
        (lhs, rhs) <- lift $ between
            (optSpacing *> pChar '(')
            (optSpacing *> pChar ')')
            (both argument (optSpacing *> pChar ',' *> argument))
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
    return $ Defn range Binary name [lhs, rhs] ret_ty body

defnFuncStatment :: KoakParser (Stmt Range)
defnFuncStatment = do
    ((name, args, ret_ty, body), range) <- withRangeT $ do
        lift $ optSpacing *> pString "def"
        name <- lift $ spacing *> identifier
        args <- lift $ between
            (optSpacing *> pChar '(')
            (optSpacing *> pChar ')')
            (sepBy0 (optSpacing *> pChar ',') argument)
        lift $ optSpacing *> pChar ':'
        ret_ty <- lift $ optSpacing *> typeName
        lift optSpacing
        body <- expressions
        lift $ optSpacing *> pChar ';'
        return (name, args, ret_ty, body)
    return $ Defn range Function name args ret_ty body

defnStatement :: KoakParser (Stmt Range)
defnStatement =
    defnUnaryStatment
    <|> defnBinaryStatment
    <|> defnFuncStatment

exprStatement :: KoakParser (Stmt Range)
exprStatement = do
    (expr, range) <- withRangeT $ do
        lift optSpacing
        expr <- expressions
        lift $ optSpacing *> pChar ';'
        return expr
    return $ Expr range expr

externStatement :: KoakParser (Stmt Range)
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
    return $ Extern range name args ret_ty

statement :: KoakParser (Stmt Range)
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
