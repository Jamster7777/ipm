module Core.SemVer
import Core.IpmError

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

incrementMajor : Version -> Version
incrementMajor (MkVersion mj mn p) = MkVersion (mj + 1) 0 0

incrementMinor : Version -> Version
incrementMinor (MkVersion mj mn p) = MkVersion mj (mn + 1) 0

incrementPatch : Version -> Version
incrementPatch (MkVersion mj mn p) = MkVersion mj mn (p + 1)

-- data Rule =
--
-- data Range  = EQ_V Version
--                   | GT_V Version
--                   | GTE_V Version
--                   | LT_V Version
--                   | LTE_V Version
--
-- data Rule
-- findCompatabileVersionRange : VersionRange -> VersionRange ->  Either IpmError VersionRange
