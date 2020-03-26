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


--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

initPkgVersions : PkgName -> Version -> PkgVersions
initPkgVersions n v =
  insert n [v] empty

initManifests : PkgName -> Version -> Manifest -> Manifests
initManifests n v m =
  insert (n, v) m empty
