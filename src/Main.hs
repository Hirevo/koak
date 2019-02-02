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
    case parse program contents of
        Parsed (ast, _) -> do
            case ast |> inferAST of
                Left err -> P.putStrLn $ show err
                Right types -> do
                    putStrLn "Success:"
                    putStrLn $ concat $ intersperse "\n" $ map show types
            exitWith ExitSuccess
        NotParsed err -> do
            err |> show
                |> putStrLn
            exitWith $ ExitFailure 84
    -- putStrLn $ show $ parse program contents
