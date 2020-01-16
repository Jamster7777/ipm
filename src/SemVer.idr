module SemVer
import IpmError

-- Major, minor, patch, prerelease, build metadata
data Version = MkVersion Integer Integer Integer

Eq Version where
  (==) (MkVersion mj1 mn1 p1) (MkVersion mj2 mn2 p2) =
    (mj1 == mj2) && (mn1 == mn2) && (p1 == p2)
  (/=) x y = not (x == y)

Ord Version where
  compare x y = ?Ord_rhs_1
  (<) x y = ?Ord_rhs_2
  (>) x y = ?Ord_rhs_3
  (<=) x y = ?Ord_rhs_4
  (>=) x y = ?Ord_rhs_5
  max x y = ?Ord_rhs_6
  min x y = ?Ord_rhs_7


-- data VersionRange = EqV Version | GtV Version | LtV Version | GteV Version
-- findCompatabileVersionRange : VersionRange -> VersionRange ->  Either IpmError VersionRange
