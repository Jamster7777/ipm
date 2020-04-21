module Core.ManifestTypes
import Semver.Range
import Semver.Version


%access public export

data PkgName = MkPkgName String String -- Namespace and name, e.g. jab36/http
data PkgSource = PkgUrl String | PkgLocal String -- TODO central repository
data ManiDep = MkManiDep PkgName PkgSource Range

record PkgConfig where
    constructor MkPkgConfig
    sourcedir  : Maybe String
    modules : Maybe (List String)
    main : Maybe String
    executable : Maybe String
    opts : Maybe String

-- TODO refactor code to use the fact this is a record now
record Manifest where
    constructor MkManifest
    name : PkgName
    deps : List ManiDep
    config : PkgConfig

%name Version v

Show PkgName where
  show (MkPkgName group name) = group ++ "/" ++ name

%name PkgName n

Eq PkgName where
  (==) (MkPkgName x z) (MkPkgName y w) = x == y && z == w

Ord PkgName where
  compare x y = compare (show x) (show y)

getDependancies : Manifest -> List ManiDep
getDependancies (MkManifest x xs z) = xs

getName : ManiDep -> PkgName
getName (MkManiDep n x y) = n

getDepNames : Manifest -> List PkgName
getDepNames (MkManifest _ ds _) = map getName ds
