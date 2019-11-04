import System

main : IO ()
main = do
  exitCode <- system "pwd"
  putStrLn $ "Exit code: " ++ show exitCode

  exitCode <- system "echo HelloWorld!; false"
  putStrLn $ "Exit code: " ++ show exitCode