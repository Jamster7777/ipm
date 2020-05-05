module PubGrub.Types.PackageTracking

import Core.ManifestTypes
import Semver.Version
import Data.SortedMap

%access public export

--------------------------------------------------------------------------------
-- Types for keeping track of package versions and manifests
--------------------------------------------------------------------------------

||| Store the available versions for each package
PkgVersions : Type
PkgVersions = SortedMap PkgName (List Version)

||| Store the parsed manifests for each package version
Manifests : Type
Manifests = SortedMap (PkgName, Version) Manifest

||| Stores the packages that currently need a decision, alongside the decision
||| level this requirement comes from (so the requirement can be removed upon
||| backtracking)
NeedDec : Type
NeedDec = SortedMap PkgName Integer

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

||| Initially, only the root package and its latest version are contained in
||| PkgVersions
initPkgVersions : PkgName -> Version -> PkgVersions
initPkgVersions n v =
  insert n [v] empty

||| Initially, the only package that needs a decision is the root package.
initNeedDec : PkgName -> NeedDec
initNeedDec n = insert n 0 empty
