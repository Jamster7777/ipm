module PubGrub.Types.Term

import Semver.Range

%access public export


data Term = Pos Range | Neg Range

not : Term -> Term
not (Pos x) = Neg x
not (Neg x) = Pos x

Eq Term where
  (==) (Pos x) (Pos y) = x == y
  (==) (Pos x) (Neg y) = False
  (==) (Neg x) (Pos y) = False
  (==) (Neg x) (Neg y) = x == y
  (/=) x       y       = not (x == y)

Show Term where
  show (Pos r) = show r
  show (Neg r) = "not " ++ (show r)
