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
import Control.Monad.State.Lazy
import Prelude as P

main :: IO ()
-- main = fib
--     |> ppllvm
--     |> toStrict
--     |> TIO.putStrLn
main = do
    [filename] <- getArgs
    contents <- P.readFile filename
    -- case parse (intoParser ) contents of
    --     Parsed (ast, _) -> do
    --         ast |> P.putStrLn
    --         exitWith ExitSuccess
    --     NotParsed err -> do
    --         err |> show
    --             |> P.putStrLn
    --         exitWith $ ExitFailure 84
    P.putStrLn $ show $ parse (evalStateT expressions defaultPrecedenceMap) contents
