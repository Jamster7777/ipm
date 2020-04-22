module Commands.Versions

import Core.Opts
import Util.FetchDep
import Util.ListExtras
import Core.IpmError
import Semver.Version

export
versions : Opts -> IO ()
versions opts =
  do  res <- listVersions'
      case res of
        Left err => putStrLn "Error reading versions - has ipm init been ran?"
        Right vs => putStrLn $ showList vs show
