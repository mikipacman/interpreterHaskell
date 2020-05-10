module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import LexGramatyka
import ParGramatyka
import SkelGramatyka
import PrintGramatyka
import AbsGramatyka

import ErrM
import Semantics

type ParseFun a = [Token] -> Err a
type Verbosity = Int

myLLexer = myLexer

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
    Bad s    -> do 
        putStrLn "\nParse Failed...\n"
        putStrV v "Tokens:"
        putStrV v $ show ts
        putStrLn s
        exitFailure
    Ok  tree -> do 
        showTree v tree
        runTree v tree
        exitSuccess


showTree :: (Show a, Print a) => Verbosity -> a -> IO ()
showTree v tree = do
    putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
    putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
    putStrLn $ unlines
        [ "usage: Call with one of the following argument combinations:"
        , "  (path_to_file)  Parse content of file silently."
        , "  --help          Display this help message."
        , "  --debug         Debug mode."
        , "  Other arguments will be ignored"
        ]
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--help"]          -> usage
        "--debug":fileNames -> (runFile 2 pProgram) (fileNames !! 0)
        fileNames           -> (runFile 0 pProgram) (fileNames !! 0)
    return ()

