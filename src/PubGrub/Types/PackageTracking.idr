module PubGrub.Types.PackageTracking

import Core.ManifestTypes
import Semver.Version
import Data.AVL.Dict

%access public export

--------------------------------------------------------------------------------
-- Types for keeping track of package versions and manifests
--------------------------------------------------------------------------------

PkgVersions : Type
PkgVersions = Dict PkgName (List Version)

Manifests : Type
Manifests = Dict (PkgName, Version) Manifest
