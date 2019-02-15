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
import Debug.Trace

import Codegen.Codegen (codegenAST)
import System.Environment (getArgs)
import Data.Text.Lazy (toStrict, unpack)
import LLVM.Pretty (ppllvm)
import Data.List (intersperse)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorString, userError, ioError, catchIOError)
import Foreign.Ptr (FunPtr, castFunPtr)
import Control.Monad (void)

import qualified Data.Map as Map
import qualified Data.Text.IO as TIO
import qualified LLVM.ExecutionEngine as EE
import qualified LLVM.Context as Ctx
import qualified LLVM.AST as AST
import qualified LLVM.Module as Mdl
import qualified LLVM.Target as Tgt
import qualified System.Process as Pcs

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int) -> IO Int

run :: FunPtr a -> IO Int
run fn = haskFun (castFunPtr fn :: FunPtr (IO Int))

jit :: Ctx.Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optlevel model ptrelim fastins
    where
        optlevel = Just 2
        model    = Nothing
        ptrelim  = Nothing
        fastins  = Nothing

printBuiltinOps :: IO ()
printBuiltinOps = do
    putStrLn "Built-in binary operators:"
    builtinBinaryOps |> Map.toList
                     |> mapM (\(name, ty) -> putStrLn ("(" ++ name ++ "): " ++ show ty))
    putStrLn ""
    putStrLn "Built-in unary operators:"
    builtinUnaryOps |> Map.toList
                    |> mapM (\(name, ty) -> putStrLn ("(" ++ name ++ "): " ++ show ty))
    putStrLn ""


main :: IO ()
main = flip catchIOError (\err -> do
        hPutStrLn stderr $ "error: " ++ ioeGetErrorString err
        exitWith (ExitFailure 84)) $ do
    args <- getArgs
    case args of
        [arg] -> do
            parseResult <- arg |> parseFile program
                               |> flip catchIOError (\_ -> ioError $ userError $ "No such source file: " ++ arg)
            case parseResult of
                Parsed (ast, _) -> -- do
                    -- putStrLn "AST:"
                    -- putStrLn (show ast)
                    -- putStrLn ""
                    case ast |> annotateAST of
                        Left err -> err |> show |> fail
                        Right types -> -- do
                            -- putStrLn "Type-checked successfully !"
                            -- putStrLn "Expression types:"
                            -- types |> map (show . annotation)
                            --       |> intersperse "\n"
                            --       |> concat
                            --       |> putStrLn
                            Ctx.withContext $ \ctx -> do
                                let mod = codegenAST types
                                mod |> ppllvm |> unpack |> putStrLn
                                Mdl.withModuleFromAST ctx mod $ \mod' ->
                                    jit ctx $ \ecjit -> EE.withModuleInEngine ecjit mod' $ \ee -> do
                                        mainfn <- EE.getFunction ee (AST.mkName "main")
                                        case mainfn of
                                            Just fn -> Control.Monad.void $ run fn
                                            Nothing -> return ()
                                        Tgt.withHostTargetMachine $ \tgt ->
                                            Mdl.writeObjectToFile tgt (Mdl.File "out.o") mod'
                                mod |> ppllvm |> unpack |> writeFile "out.ll"
                                Pcs.callCommand "cc out.o"
                err -> err |> show |> fail
        _ -> fail $ "Unexpected number of arguments: expected 1 but got " ++ show (length args)
