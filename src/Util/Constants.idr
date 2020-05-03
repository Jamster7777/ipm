module Util.Constants

%access public export

MANIFEST_FILE_NAME : String
MANIFEST_FILE_NAME = "ipm.json"

BUILD_FILE_NAME : String
BUILD_FILE_NAME = "ipm-build.ipkg"

LOCK_FILE_NAME : String
LOCK_FILE_NAME = "ipm-lock.json"

TEMP_DIR : String
TEMP_DIR = "/home/jamie/ipm-temp/"

PUBLISH_TEMPLATE_MESSAGE_LOCATION : String
PUBLISH_TEMPLATE_MESSAGE_LOCATION = "/home/jamie/Documents/uni/diss/ipm/assets/default-publish-message"

PR_SEP : String
PR_SEP = "\n--------------------------\n"

VERBOSE_FNAME_WIDTH : Nat
VERBOSE_FNAME_WIDTH = fromIntegerNat 30
