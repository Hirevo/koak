{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Annotation
import Misc
import Types hiding (void)
import Parser.Lib hiding (Parser)
import Parser.Lang hiding (argument)
-- import Passes.Annotation
import Passes.Inference as I
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Exception.Base
import System.Exit
import Debug.Trace
import Options.Applicative

import Codegen.Codegen (codegenAST)
import System.Environment (getArgs)
import Data.Text.Lazy (toStrict, unpack)
import LLVM.Pretty (ppllvm)
import Data.List (intercalate)
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
import qualified LLVM.PassManager as Pm
import qualified LLVM.Target as Tgt
import qualified System.Process as Pcs

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int) -> IO Int

run :: FunPtr a -> IO Int
run fn = haskFun (castFunPtr fn :: FunPtr (IO Int))

withJIT :: Ctx.Context -> (EE.MCJIT -> IO a) -> IO a
withJIT ctx = EE.withMCJIT ctx optlevel model ptrelim fastins
    where
        optlevel = Just 3
        model    = Nothing
        ptrelim  = Nothing
        fastins  = Nothing

printBuiltinOps :: IO ()
printBuiltinOps = do
    putStrLn "Built-in binary operators:"
    builtinBinaryOps |> mapM (\(name, ty) -> putStrLn ("(" <> name <> "): " <> show ty))
    putStrLn ""
    putStrLn "Built-in unary operators:"
    builtinUnaryOps |> mapM (\(name, ty) -> putStrLn ("(" <> name <> "): " <> show ty))
    putStrLn ""

data Options = Options {
    output      :: Maybe String,
    llvm_ir     :: Maybe String,
    bitcode     :: Maybe String,
    ast         :: Bool,
    builtins    :: Bool,
    silent      :: Bool,
    filename    :: String
}

optsParser :: Parser Options
optsParser = Options
    <$> optional (strOption $ long "output"
                           <> short 'o'
                           <> metavar "FILE"
                           <> help "Write executable to FILE")
    <*> optional (strOption $ long "llvm-ir"
                           <> metavar "FILE"
                           <> help "Write LLVM-IR to FILE")
    <*> optional (strOption $ long "bitcode"
                           <> metavar "FILE"
                           <> help "Write LLVM bitcode to FILE")
    <*> switch (long "ast"
                <> short 'a'
                <> help "Display the internal AST")
    <*> switch (long "builtins"
                <> short 'b'
                <> help "Display the builtin operators")
    <*> switch (long "silent"
                <> short 's'
                <> help "Disable the JIT execution")
    <*> argument str (metavar "SOURCE_FILE")

main :: IO ()
main = flip catchIOError (\err -> do
        hPutStrLn stderr $ "error: " <> ioeGetErrorString err
        exitWith (ExitFailure 84)) $ do

    opts <- execParser $ info
        (optsParser <**> helper)
        (fullDesc <> progDesc "Compiles a KOAK source file (potentially executes it with a JIT)"
                  <> header "koak - The KOAK language compiler"
                  <> failureCode 84)
    let arg = filename opts
    parseResult <- arg |> parseFile program
                       |> flip catchIOError (\_ -> ioError $ userError $ "No such source file: " <> arg)
    case builtins opts of
        True -> printBuiltinOps
        False -> return ()
    case parseResult of
        Parsed (parsed, _) ->
            case parsed |> infer of
                Left err -> err |> show |> fail
                Right types -> do
                    case ast opts of
                        True -> do
                            putStrLn "AST:"
                            types |> show |> putStrLn
                            putStrLn ""
                        False -> return ()
                    Ctx.withContext $ \ctx -> do
                        let mod = codegenAST types
                        mod |> ppllvm |> unpack |> putStrLn
                        case llvm_ir opts of
                            Just ir_file -> do
                                let ir = mod |> ppllvm |> unpack
                                ir |> writeFile ir_file
                                case output opts of
                                    Just file -> void (ir |> Pcs.readCreateProcess
                                        (Pcs.shell $ "cc -O3 -xir - -o '" <> file <> "' -lm"))
                                    Nothing -> return ()
                            Nothing -> case output opts of
                                    Just file ->
                                        void (Pcs.readCreateProcess
                                            (Pcs.shell $ "cc -O3 -xir - -o '" <> file <> "' -lm")
                                            (mod |> ppllvm |> unpack))
                                    Nothing -> return ()
                        Mdl.withModuleFromAST ctx mod $ \mod' -> do
                            case bitcode opts of
                                Just bc_file -> Mdl.writeBitcodeToFile (Mdl.File bc_file) mod'
                                Nothing -> return ()
                            case silent opts of
                                True -> return ()
                                False -> withJIT ctx $ \ecjit -> EE.withModuleInEngine ecjit mod' $ \ee -> do
                                    mainfn <- EE.getFunction ee (AST.mkName "main")
                                    case mainfn of
                                        Just fn -> Control.Monad.void $ run fn
                                        Nothing -> return ()
        err@(NotParsed Pos{ file = Just file, line, column } _) -> do
            contents <- readFile file
            let ln = contents |> lines |> (!! (line - 1))
            let col = column - 1
            let padding = '~' |> repeat |> take col
            let line_nb = show line
            let pre_pad = take (length line_nb) (repeat ' ')
            fail (show err <> "\n " <> pre_pad <> " |\n " <> line_nb <> " | " <> ln <> "\n "
                <> pre_pad <> " | " <> take col padding <> "^")
        err -> err |> show |> fail
        -- _ -> fail $ "Unexpected number of arguments: expected 1 but got " <> show (length args)
