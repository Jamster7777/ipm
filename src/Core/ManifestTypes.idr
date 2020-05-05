module Core.ManifestTypes
import Semver.Range
import Semver.Version


%access public export

||| A package name is comprised of a group name and a package name, e.g.
||| MkPkgName "jamie" "http" corresponds to the package jamie/http.
data PkgName = MkPkgName String String

||| A package source can be specified as a URL or a local path
data PkgSource = PkgUrl String | PkgLocal String

||| A dependency specified in a manifest. Stores the package name, source, and
||| allowed version range.
data ManiDep = MkManiDep PkgName PkgSource Range

||| The PkgConfig record stores all metadata not required by version solving.
record PkgConfig where
    constructor MkPkgConfig
    sourcedir  : Maybe String
    modules : Maybe (List String)
    main : Maybe String
    executable : Maybe String
    opts : Maybe String

||| A manifest requires a name and list of dependencies for version solving. It
||| also stores the metadata fields to be used after version solving.
record Manifest where
    constructor MkManifest
    name : PkgName
    deps : List ManiDep
    config : PkgConfig

-- For ease of development set the default name for SemVer versions to v
%name Version v

-- For ease of development set the default name for PkgNames to n
%name PkgName n

Show PkgName where
  show (MkPkgName group name) = group ++ "/" ++ name

Eq PkgName where
  (==) (MkPkgName x z) (MkPkgName y w) = x == y && z == w

Ord PkgName where
  compare x y = compare (show x) (show y)

||| Extract the name from a dependency
getName : ManiDep -> PkgName
getName (MkManiDep n x y) = n

||| List the names of the dependencies for a manifest
getDepNames : Manifest -> List PkgName
getDepNames (MkManifest _ ds _) = map getName ds
