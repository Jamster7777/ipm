module Commands.Push

import Core.Opts
import Core.IpmError
import Util.Bash


||| The reason the ipm push command exists is because in order to push new tags
||| to a remote repository, the --follow-tags option is required. A lot of git
||| users may not know this so it is abstracted over for them.
export
push : Opts -> IO (Either IpmError ())
push opts =
  bashCommandErr
    ("git push --follow-tags")
    "Error pushing new version to remote - has a remote repository been configured?"
