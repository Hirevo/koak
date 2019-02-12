module Main where

import Annotation
import Misc
import Types
import Parser.Lib
import Parser.Lang
import Passes.Annotation
import Passes.Inference
import Control.Monad.State.Lazy
import Control.Exception.Base
import System.Exit

import Codegen.Codegen (fib)
import System.Environment (getArgs)
import Data.Text.Lazy (toStrict)
import LLVM.Pretty (ppllvm)
import Data.List (intersperse)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorString, userError, ioError, catchIOError)

import qualified Data.Map as Map
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
            putStrLn "Built-in binary operators:"
            builtinBinaryOps |> Map.toList
                             |> map (\(name, ty) -> putStrLn ("(" ++ name ++ "): " ++ show ty))
                             |> sequence_
            putStrLn ""
            putStrLn "Built-in unary operators:"
            builtinUnaryOps |> Map.toList
                            |> map (\(name, ty) -> putStrLn ("(" ++ name ++ "): " ++ show ty))
                            |> sequence_
            putStrLn ""
            parseResult <- arg |> parseFile program
                               |> flip catchIOError (\_ -> ioError $ userError $ "No such source file: " ++ arg)
            case parseResult of
                Parsed (ast, _) -> do
                    -- putStrLn "AST:"
                    -- putStrLn (show ast)
                    -- putStrLn ""
                    case ast |> annotateAST of
                        Left err -> err |> show |> fail
                        Right types -> do
                            putStrLn "Type-checked successfully !"
                            putStrLn "Expression types:"
                            types |> map (show . annotation)
                                  |> intersperse "\n"
                                  |> concat
                                  |> putStrLn
                err -> err |> show |> fail
        _ -> fail $ "Unexpected number of arguments: expected 1 but got " ++ show (length args)
