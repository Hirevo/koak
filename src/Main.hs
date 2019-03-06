{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Misc
import Types hiding (void)
import Parser.Lib hiding (Parser)
import Parser.Lang hiding (argument)
-- import Passes.Deserialization
import Passes.Inference as I
import Passes.Deserialization as Des
import Control.Monad.State.Lazy
import System.Exit
import Options.Applicative

import Codegen.Codegen (codegenAST)
import Data.Text.Lazy (unpack)
import LLVM.Pretty (ppllvm)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (ioeGetErrorString, userError, ioError, catchIOError)
import Foreign.Ptr (FunPtr, castFunPtr)
import Control.Monad (void)

import qualified LLVM.ExecutionEngine as EE
import qualified LLVM.Context as Ctx
import qualified LLVM.AST as AST
import qualified LLVM.Module as Mdl
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
    forM_ builtinBinaryOps $ \(name, ty) ->
        putStrLn ("(" <> name <> "): " <> show ty)
    putStrLn ""
    putStrLn "Built-in unary operators:"
    forM_ builtinUnaryOps $ \(name, ty) ->
        putStrLn ("(" <> name <> "): " <> show ty)
    putStrLn ""
    putStrLn "Built-in functions:"
    forM_ builtinFunctions $ \(name, ty) ->
        putStrLn (name <> ": " <> show ty)
    putStrLn ""

data Options = Options {
    output      :: Maybe String,
    llvm_ir     :: Maybe String,
    bitcode     :: Maybe String,
    deserialize :: Bool,
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
    <*> switch (long "deserialize"
                           <> short 'd'
                           <> help "Display the deserialized result")
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
            -- putStrLn "Deserialized:"
            -- putStrLn $ deserializeAST parsed
            case parsed |> infer of
                Left err -> err |> show |> fail
                Right types -> do
                    case ast opts of
                        True -> do
                            putStrLn "AST:"
                            types |> show |> putStrLn
                            putStrLn ""
                        False -> return ()
                    case deserialize opts of
                        True -> do
                            putStrLn "Deserialized:"
                            putStrLn $ Des.deserializeAST parsed
                            putStrLn ""
                        False -> return ()
                    Ctx.withContext $ \ctx -> do
                        let mdl = codegenAST types
                        -- mdl |> ppllvm |> unpack |> putStrLn
                        case llvm_ir opts of
                            Just ir_file -> do
                                let ir = mdl |> ppllvm |> unpack
                                ir |> writeFile ir_file
                                case output opts of
                                    Just file -> void (ir |> Pcs.readCreateProcess
                                        (Pcs.shell $ "cc -O3 -xir - -o '" <> file <> "' -lm"))
                                    Nothing -> return ()
                            Nothing -> case output opts of
                                    Just file ->
                                        void (Pcs.readCreateProcess
                                            (Pcs.shell $ "cc -O3 -xir - -o '" <> file <> "' -lm")
                                            (mdl |> ppllvm |> unpack))
                                    Nothing -> return ()
                        Mdl.withModuleFromAST ctx mdl $ \mdl' -> do
                            case bitcode opts of
                                Just bc_file -> Mdl.writeBitcodeToFile (Mdl.File bc_file) mdl'
                                Nothing -> return ()
                            case silent opts of
                                True -> return ()
                                False -> withJIT ctx $ \ecjit -> EE.withModuleInEngine ecjit mdl' $ \ee -> do
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
