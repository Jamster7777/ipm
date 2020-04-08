module Util.Constants

%access public export

MANIFEST_FILE_NAME : String
MANIFEST_FILE_NAME = "ipm.json"

LOCK_FILE_NAME : String
LOCK_FILE_NAME = "lock.ipkg"

TEMP_DIR : String
TEMP_DIR = "/home/jamie/ipm-temp/"

PACKAGE_INSTALL_LOCATION : String
PACKAGE_INSTALL_LOCATION = "~/ipkg/"

PUBLISH_TEMPLATE_MESSAGE_LOCATION : String
PUBLISH_TEMPLATE_MESSAGE_LOCATION = "/home/jamie/Documents/uni/diss/ipm/assets/default-publish-message.txt"

PR_SEP : String
PR_SEP = "\n--------------------------\n"

VERBOSE_FNAME_WIDTH : Nat
VERBOSE_FNAME_WIDTH = fromIntegerNat 30
