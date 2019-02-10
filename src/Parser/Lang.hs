module Parser.Lang where

import Control.Applicative
import Parser.Lib
import Data.Char (isSpace)
import Data.List (sortBy)
import Control.Monad.State.Lazy
import Annotation
import Misc
import Debug.Trace

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
data Type =
    IntType
    | FloatType
    | VoidType
    deriving (Show, Eq)
data Arg = Arg {
    arg_name :: String,
    arg_type :: Type
} deriving (Show, Eq)
data Arity =
    Unary
    | Binary
    deriving (Show, Eq)
data Defn a =
    Op (Ann a (OpDefn a))
    | Fn (Ann a (FnDefn a))
    deriving (Show, Eq)
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

expressions :: KoakParser (Expr ())
expressions =
    (For . Ann () <$> forExpression)
    <|> (If . Ann () <$> ifExpr)
    <|> (While . Ann () <$> whileExpr)
    <|> expression

forExpression :: KoakParser (ForExpr ())
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
        for_init = Ann () init,
        for_cond = Ann () cond,
        for_oper = Ann () oper,
        for_body = Ann () body
    }

ifExpr :: KoakParser (IfExpr ())
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
        if_cond = Ann () if_expr,
        if_then = Ann () then_body,
        if_else = Ann () <$> else_body
    }

whileExpr :: KoakParser (WhileExpr ())
whileExpr = do
    lift $ optSpacing *> pString "while"
    lift spacing
    while_expr <- expression
    lift $ spacing *> pString "do"
    lift spacing
    body <- expressions
    return $ WhileExpr {
        while_cond = Ann () while_expr,
        while_body = Ann () body
    }

parseBinOp :: Expr () -> Int -> KoakParser (Expr ())
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
             let expr = Bin $ Ann () $ BinExpr {
                 bin_op = op,
                 bin_lhs = Ann () lhs,
                 bin_rhs = Ann () rhs
             }
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
                return $ Bin $ Ann () $ BinExpr {
                    bin_op = op,
                    bin_lhs = Ann () lhs,
                    bin_rhs = Ann () rhs
                }
             outer lhs minPrec
    lift $ outer lhs minPrec

expression :: KoakParser (Expr ())
expression = do
    precMap <- get
    lift optSpacing
    lhs <- unary
    parseBinOp lhs 0

unary :: KoakParser (Expr ())
unary = do
    precMap <- get
    lift optSpacing
    let p1 = do
         operator <- lift $ choice $ map (pString . fst) $ un_prec precMap
         lift optSpacing
         operand <- unary
         return $ Un $ Ann () $ UnExpr {
             un_op = operator,
             un_rhs = Ann () operand
         }
    p1 <|> postfix

postfix :: KoakParser (Expr ())
postfix = do
    precMap <- get
    lift optSpacing
    let ident = lift $ Ident . Ann () <$> identifier
    let lit = lift $ Lit . Ann () <$> literal
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
         return $ Call $ Ann () $ CallExpr {
             call_ident = name,
             call_args = Ann () <$> args
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

defnUnaryStatment :: KoakParser (OpDefn ())
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
        opdefn_args = [Ann () arg],
        opdefn_ret_ty = ret_ty,
        opdefn_body = Ann () exprs
    }

defnBinaryStatment :: KoakParser (OpDefn ())
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
        opdefn_args = [Ann () arg1, Ann () arg2],
        opdefn_ret_ty = ret_ty,
        opdefn_body = Ann () exprs
    }

defnFuncStatment :: KoakParser (FnDefn ())
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
        fndefn_args = Ann () <$> args,
        fndefn_ret_ty = ret_ty,
        fndefn_body = Ann () exprs
    }

defnStatement :: KoakParser (Defn ())
defnStatement =
    (Op . Ann () <$> defnUnaryStatment)
    <|> (Op . Ann () <$> defnBinaryStatment)
    <|> (Fn . Ann () <$> defnFuncStatment)

exprStatement :: KoakParser (Expr ())
exprStatement = do
    lift optSpacing
    expr <- expressions
    lift $ optSpacing *> pChar ';'
    return expr

statement :: KoakParser (Stmt ())
statement =
    (DefnStmt . Ann () <$> defnStatement)
    <|> (ExprStmt . Ann () <$> exprStatement)

statements :: KoakParser (AST ())
statements = do
    lift optSpacing
    stmt <- Ann () <$> statement
    s <- get
    lift $ fallback [stmt] $ do
        stmts <- evalStateT statements s
        return (stmt : stmts)

program :: Parser (AST ())
program = evalStateT allStatements defaultPrecedenceMap
    where
        allStatements :: KoakParser (AST ())
        allStatements = do
            stmts <- statements
            lift $ optSpacing *> pEnd
            return stmts
