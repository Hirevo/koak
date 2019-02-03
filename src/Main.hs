module Main where

import System.Environment (getArgs)
import System.Exit
import Parser.Lib
import Parser.Lang
import Checker
import Misc
import LLVM.AST as AST
import Codegen (fib)
import qualified Data.Text.IO as TIO
import Data.Text.Lazy (toStrict)
import LLVM.Pretty (ppllvm)
import Control.Monad.State.Lazy
import Data.List (intersperse)
import Prelude as P
import Control.Exception.Base

main :: IO ()
-- main = fib
--     |> ppllvm
--     |> toStrict
--     |> TIO.putStrLn
main = do
    [filename] <- getArgs
    contents <- readFile filename
    putStrLn "Built-in binary operators:"
    putStrLn $ "  (+): " ++ show (TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat] TFloat])
    putStrLn $ "  (-): " ++ show (TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat] TFloat])
    putStrLn $ "  (*): " ++ show (TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat] TFloat])
    putStrLn $ "  (/): " ++ show (TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat] TFloat])
    putStrLn $ "  (<): " ++ show (TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat] TInt])
    putStrLn $ "  (>): " ++ show (TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat] TInt])
    putStrLn $ "  (==): " ++ show (TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat] TInt])
    putStrLn $ "  (!=): " ++ show (TUni [TFun [TInt, TInt] TInt, TFun [TFloat, TFloat] TInt])
    putStrLn $ "  (:): " ++ show (TUni [TFun [TUni [TInt, TFloat], TInt] TInt, TFun [TUni [TInt, TFloat], TFloat] TFloat])
    putStrLn ""
    putStrLn "Built-in unary operators:"
    putStrLn $ "  (!): " ++ show (TUni [TFun [TInt] TInt, TFun [TFloat] TInt])
    putStrLn $ "  (-): " ++ show (TUni [TFun [TInt] TInt, TFun [TFloat] TFloat])
    putStrLn ""
    case parse program contents of
        Parsed (ast, _) -> do
            case ast |> inferAST of
                Left err -> P.putStrLn $ show err
                Right types -> do
                    putStrLn "Type-checked successfully !"
                    putStrLn "Expression types:"
                    putStrLn $ concat $ intersperse "\n" $ map show types
            exitWith ExitSuccess
        NotParsed err -> do
            err |> show
                |> putStrLn
            exitWith $ ExitFailure 84
    -- putStrLn $ show $ parse program contents
