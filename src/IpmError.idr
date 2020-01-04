module IpmError

%access public export

data IpmError = BashError String | PublishError String

Show IpmError where
  show (BashError x)      = ?asdf_1
  show (PublishError x)   = x
