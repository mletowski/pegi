module  Main  where

import  Control.Monad       (forM_, unless)
import  Data.List           ((\\), isPrefixOf)
import  System.Environment  (getArgs)
import  System.Exit         (exitFailure, exitSuccess)

import  FGParser            (spGrammar)
import  FirstGrammar        (Grammar, parse)
import  SParser             (sparse, SPResult(..))

{--  The program help message  --}
helpText :: String
helpText = "pegi ver 1.0\n\
           \usage   :  pegi  [options]  grammarfile  [file ...]\n\
           \options :    -h   - help"

{--  Terminate program with error message  --}
pegiFail :: String -> IO ()
pegiFail message = do 
                        putStrLn message
                        exitFailure

{--  Interprete special commandline options --}
doOption :: String -> IO ()
doOption  "-h" = do
                        putStrLn  helpText
                        exitSuccess
doOption  opt = unless  (opt == "-h") $
                     pegiFail ("Unknown commandline option " ++ opt)

{--  Show parsgin error message  --}
showError :: String -> String -> String -> String -> String
showError  fname  msg  s r = 
        let  parsed = take (length s - length r) s
             ls = lines (parsed ++ ".")
             line = length ls
             pos = length (last ls)
        in  fname ++ ":" ++ show line ++ ":" ++ show pos ++ ": error: " ++ msg

{--  Show grammar --}
showGrammar :: String -> Grammar -> String
showGrammar fname g = fname ++ ":\n" ++ show g
              
{--  Parse grammar and display the result, 
     do no data parsing.  
 --}    
doGrammar :: [String] -> String -> IO ()
doGrammar  _options fname = do 
        s <- readFile fname
        case  sparse spGrammar s  of
                SPError  msg  rest -> putStrLn $ showError fname msg s rest
                SPOK _ _ -> putStrLn $ "Grammar parsed, no file arguments given"

{--  Run the grammar on given file --}
runGrammar :: Grammar -> String -> IO ()
runGrammar  grammar  fname = do
        s <- readFile  fname
        case  parse grammar "goal" s  of
                SPError err rest -> putStrLn $ showError fname err s rest
                SPOK r _ -> putStr r

{--  Parse grammar and then parse given files  --}
doParsing :: [String] -> String -> [String] -> IO ()
doParsing  _options gname files = do
        s <- readFile gname
        case  sparse spGrammar s  of
                SPError  msg  rest -> putStrLn $ showError gname msg s rest
                SPOK grammar _ -> forM_ files (runGrammar grammar)

main :: IO ()
main = getArgs >>= runpegi  where
        runpegi [] = putStrLn helpText
        runpegi args = let  options = filter ("-" `isPrefixOf`) args
                            files = args \\ options
                       in do
                            forM_ options doOption

                            case  files  of
                               [] -> pegiFail "Bad usage"
                               [gname] -> doGrammar options gname
                               (gname: files') -> doParsing options gname files'
                                
