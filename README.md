An attempt at a package manager for Idris, for my Senior Honours Project at the University of St Andrews.

# How to set up this package manager for use

Below are instructions on getting ipm v0.1.0 ready to use. Apologies for the
rough and ready approach - this is one of the processes which would have to
improve in future versions of the package manager.

Prerequisites:
- Make sure git is installed on your system. Instructions for installing it can
  be found here:
  https://git-scm.com/book/en/v2/Getting-Started-Installing-Git
- Make sure Idris 1 is installed on your system. Instructions for installing it
  can be found here:
  https://www.idris-lang.org/pages/download.html

Installation procedure:
- From this directory, execute the following commands to install the required
  packages to run ipm:
  ```
  $ cd requiredPackages/idris-containers
  $ idris --install containers.ipkg
  $ cd ../lightyear
  $ idris --install lightyear
  $ cd ../semver
  $ idris --install semver
  ```
- Modify the 2 constants at the top of `ipm/src/Util/Constants.idr` to suit your
  system.

- From the ipm directory, run the following command. This should generate an
  ipm executable.
  ```
  $ idris --build ipm.ipkg
  ```
- It is recommended that you add the ipm directory to your system PATH variable,
  so that the ipm executable can be accessed simple by using 'ipm' from
  anywhere in your system.

ipm should now be ready to run! Run `ipm --help` for usage instructions.
