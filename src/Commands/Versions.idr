module Commands.Versions

import Core.Opts
import Util.FetchDep
import Util.ListExtras
import Core.IpmError
import Semver.Version

export
versions : Opts -> IO (Either IpmError ())
versions opts =
  do  res <- listVersions'
      case res of
        Left err => pure (Left (GenericError "Error reading versions - has ipm init been ran?"))
        Right vs => do  putStrLn $ showList vs show
                        pure $ Right ()
