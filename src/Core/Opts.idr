module Opts

public export
record Opts where
  constructor MkOpts
  verbose : Bool
  dryRun : Bool
