module Main

import IO.CommandParse
import Core.IpmError
import Data.SortedSet
import Core.Opts
import System

main : IO ()
main = do (_ :: cmdStr :: rest)
                <- getArgs
                |  _ => putStrLn "No command supplied. Run 'ipm --help' for a list of commands."
          if
            cmdStr `elem` ["-h", "--help"]
          then
            help
          else
            do  let Just cmd
                    = matchCmd cmdStr
                    | Nothing => putStrLn ("'" ++ cmdStr ++ "' is not a valid command. Run 'ipm --help' for a list of commands.")
                let Right opts
                    = matchOpts rest empty
                    | Left err => putStrLn (show err)
                res <- cmd opts
                case res of
                  Left err => do  putStrLn (show err)
                                  exit 1
                  Right () => exit 0
