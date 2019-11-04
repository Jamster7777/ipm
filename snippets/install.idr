import locktypes

install_package : (Dependancy, Version) -> IO ()


dependancies_install : List (Dependancy, Version) -> IO ()
dependancies_install [] = putStrLn "All installed."
dependancies_install (x :: xs) = do
    install_package x
    dependancies_install xs



-- main : IO ()
-- main = do
--   exitCode <- system "pwd"
--   putStrLn $ "Exit code: " ++ show exitCode

--   exitCode <- system "echo HelloWorld!; false"
--   putStrLn $ "Exit code: " ++ show exitCode