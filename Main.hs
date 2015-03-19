module Main where

import ILP
import Parser
import Pretty
import System.Console.Haskeline
import Prelude.Extras
import Data.List
import System.Environment
import qualified Data.Map as Map

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
              case parseCommand input of
                Print -> mapM_ (mapM_ (outputStrLn . pretty)) (Map.elems database) >> loop database
                Help -> outputStrLn helpMessage >> loop database
                Quit -> outputStrLn "Bye."
                Add xs ->
                  case parseClause xs of
                    Left e -> do
                      outputStrLn $ "Parse error: " ++ e
                      loop database
                    Right clause -> loop (addToDatabase clause database)
                Remove xs ->
                  case parseQuery xs of
                    Left e -> do
                      outputStrLn $ "Parse error: " ++ e
                      loop database
                    Right (Check sym vars') ->
                      loop (Map.update (\xs -> filterMap $ filter (\(Clause _ vars _) -> not (vars == vars')) xs) sym database)
                        where
                          filterMap [] = Nothing
                          filterMap xs = Just xs
                Exec xs ->
                  case parseQuery xs of
                    Left e -> do
                      outputStrLn $ "Parse error: " ++ e
                      loop database
                    Right query ->
                      case solveAll query database of
                        [] -> outputStrLn "No\n." >> loop database
                        rs -> mapM_ (\xs -> printEnv xs >> outputStrLn ".") rs >> loop database

helpMessage :: String
helpMessage = " :+ clause\tAdds a clause to the database\n\
              \ :- clause\tRemoves a clause from the database\n\
              \ :p\t\tPrint the clause database\n\
              \ :q\t\tQuits\n\
              \ :h\t\tPrint this message"

data Command = Print
             | Help
             | Quit
             | Add String
             | Remove String
             | Exec String

parseCommand :: String -> Command
parseCommand (':':'q':_)  = Quit
parseCommand (':':'h':_)  = Help
parseCommand (':':'p':_)  = Print
parseCommand (':':'+':xs) = Add xs
parseCommand (':':'-':xs) = Remove xs
parseCommand xs           = Exec xs

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
  then repl Map.empty
  else do
    p <- parseFile (args !! 0)
    case p of
      Left e -> putStrLn $ "Error parsing file: " ++ e
      Right database -> repl database
