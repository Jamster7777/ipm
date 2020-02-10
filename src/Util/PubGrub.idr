module Util.PubGrub
import Semver.Range
import Semver.Version
import Core.ManifestTypes
import Core.PubGrubTypes
import Core.IpmError


-- Main algorithm
pubGrub : Manifest -> Either IpmError Lock
