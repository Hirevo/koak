module Main where

import System.Environment (getArgs)
import System.Exit
import Parser.Lib
import Parser.Lang
import Misc
import LLVM.AST as AST
import Codegen (fib)
import Data.Text.IO as TIO
import Data.Text.Lazy (toStrict)
import LLVM.Pretty (ppllvm)

main :: IO ()
main = fib
    |> ppllvm
    |> toStrict
    |> TIO.putStrLn
