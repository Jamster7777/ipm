import System

-- TODO rename to init
main : IO ()
main = do exitCode <- system "[ -d \".git/\" ]"
          if exitCode == 0
          then putStrLn "No git repository found. Initialise a new one?"
            exitCode <- system "git init"
          else putStrLn $ "test"
          putStrLn "test2"
