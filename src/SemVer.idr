module SemVer
import IpmError

-- Major, minor, patch, prerelease, build metadata
data Version = MkVersion Integer Integer Integer

Eq Version where
  (==) (MkVersion mj1 mn1 p1) (MkVersion mj2 mn2 p2) =
    (mj1 == mj2) && (mn1 == mn2) && (p1 == p2)
  (/=) x y = not (x == y)

Ord Version where
  compare (MkVersion mj1 mn1 p1) (MkVersion mj2 mn2 p2) =
    case (compare mj1 mj2) of
      LT => LT
      GT => GT
      EQ => case (compare mn1 mn2) of
        LT => LT
        GT => GT
        EQ => case (compare p1 p2) of
          LT => LT
          GT => GT
          EQ => EQ


-- data VersionRange = EqV Version | GtV Version | LtV Version | GteV Version
-- findCompatabileVersionRange : VersionRange -> VersionRange ->  Either IpmError VersionRange
