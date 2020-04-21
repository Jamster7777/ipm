module Main

import IO.CommandParse

main : IO ()
main = do args <- getArgs
          let Just cmdStr
              = index' 1 args
              | Nothing => putStrLn "No command supplied. Run 'ipm --help' for a list of commands."
          let Just cmd
              = matchCmd cmdStr
              | Nothing => putStrLn ("'" ++ cmdStr ++ "' is not a valid command. Run 'ipm --help' for a list of commands.")
          cmd
