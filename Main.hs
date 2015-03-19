module Main where

import ILP
import Parser
import System.Console.Haskeline
import Prelude.Extras
import Data.List
import System.Environment

parseFile file = do
  f <- readFile file
  return $ parse f

repl :: Database -> IO ()
repl database = runInputT settings (loop database)
    where
      showVar (Var a) = a
      showVar (Atom a) = a
      printEnv [] = outputStrLn "Yes"
      printEnv env = mapM_ (\(a,b) -> outputStrLn $ showVar a ++ " = " ++ showVar b) env
      settings = defaultSettings {historyFile = Just ".ilp_history"}
      loop database = do
          minput <- getInputLine "?- "
          case minput of
            Nothing -> outputStrLn "Bye."
            Just input ->
              case parseQueryOrClause input of
                Left e -> do
                  outputStrLn $ "Parse error: " ++ e
                  loop database
                Right qOrC ->
                  case qOrC of
                    Left clause -> loop $ addToDatabase clause database
                    Right query ->
                      case solveAll query database of
                        [] -> outputStrLn "No\n." >> loop database
                        rs -> mapM_ (\xs -> printEnv xs >> outputStrLn ".") rs >> loop database

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
  then putStrLn "usage: ilp program"
  else do
    p <- parseFile (args !! 0)
    case p of
      Left e -> putStrLn $ "Error parsing file: " ++ e
      Right database -> repl database
