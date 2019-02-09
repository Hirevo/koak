module Main where

import System.Exit
import Parser.Lib
import Parser.Lang
import Checker
import Misc
import Control.Monad.State.Lazy
import Control.Exception.Base

import Codegen (fib)
import System.Environment (getArgs)
import Data.Text.Lazy (toStrict)
import LLVM.Pretty (ppllvm)
import Data.List (intersperse)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorString, userError, ioError, catchIOError)

import qualified LLVM.AST as AST
import qualified Data.Text.IO as TIO

main :: IO ()
-- main = fib
--     |> ppllvm
--     |> toStrict
--     |> TIO.putStrLn
main = flip catchIOError (\err -> do
        hPutStrLn stderr $ "error: " ++ ioeGetErrorString err
        exitWith (ExitFailure 84)) $ do
    args <- getArgs
    case args of
        [arg] -> do
            contents <- catchIOError (readFile arg) $ \_ -> ioError $ userError $ "No such source file: " ++ arg
            putStrLn "Built-in binary operators:"
            sequence_ $ map (\(name, ty) -> putStrLn ("(" ++ name ++ "): " ++ show ty)) (bin_ops defaultEnv)
            putStrLn ""
            putStrLn "Built-in unary operators:"
            sequence_ $ map (\(name, ty) -> putStrLn ("(" ++ name ++ "): " ++ show ty)) (un_ops defaultEnv)
            putStrLn ""
            case parse program contents of
                Parsed (ast, _) ->
                    putStrLn "AST:" >>
                    putStrLn (show ast) >>
                    case ast |> inferAST of
                        Left err -> fail $ show err
                        Right types -> do
                            putStrLn "Type-checked successfully !"
                            putStrLn "Expression types:"
                            putStrLn $ concat $ intersperse "\n" $ map show types
                NotParsed pos err -> fail $ "ParseError (at " ++ show pos ++ "): " ++ show err
        _ -> fail $ "Unexpected number of arguments: expected 1 but got " ++ show (length args)
