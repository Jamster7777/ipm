module Install

defaultPath : String
defaultPath = "~/ipm/packages/"

install : IO String -> IO ()
install path = ?install_rhs
