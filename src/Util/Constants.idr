module Util.Constants
import Util.Paths

%access public export

--------------------------------------------------------------------------------
-- Should be edited by the user for installation
--------------------------------------------------------------------------------

||| Input the location to place the ipm executables folder, as well as the ipm
||| temporary install folder.
LOCATION_FOR_IPM_FOLDERS : String
LOCATION_FOR_IPM_FOLDERS = "/home/jamie/"


||| Input the path to the parent directory of this source code (run 'pwd' from
||| the parent directory and paste the output here).
LOCATION_OF_THIS_SOURCE_CODE : String
LOCATION_OF_THIS_SOURCE_CODE = "/home/jamie/Documents/uni/diss/ipm/"

--------------------------------------------------------------------------------
-- Shouldn't be editied
--------------------------------------------------------------------------------

MANIFEST_FILE_NAME : String
MANIFEST_FILE_NAME = "ipm.json"

BUILD_FILE_NAME : String
BUILD_FILE_NAME = "ipm-build.ipkg"

LOCK_FILE_NAME : String
LOCK_FILE_NAME = "ipm-lock.json"

TEMP_DIR : String
TEMP_DIR = (cleanFilePath LOCATION_FOR_IPM_FOLDERS) ++ "ipm-temp/"

EXECUTABLES_FOLDER : String
EXECUTABLES_FOLDER = (cleanFilePath LOCATION_FOR_IPM_FOLDERS) ++ "ipm-bin/"

PUBLISH_TEMPLATE_MESSAGE_LOCATION : String
PUBLISH_TEMPLATE_MESSAGE_LOCATION = (cleanFilePath LOCATION_OF_THIS_SOURCE_CODE) ++ "assets/default-publish-message"

PR_SEP : String
PR_SEP = "\n--------------------------\n"

VERBOSE_FNAME_WIDTH : Nat
VERBOSE_FNAME_WIDTH = fromIntegerNat 30
