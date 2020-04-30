#!/home/jamie/Documents/uni/diss/ipm/python-venv-ipm/bin/python3.7

import argparse, sys, os, json

# Argument parsing is inspired by the relevant answer here:
# https://stackoverflow.com/questions/14360389/getting-file-path-from-command-line-argument-in-python

arg_parser = argparse.ArgumentParser(description="Generate a set of git repos for testing the Idris package manager on.")
arg_parser.add_argument(
    '-c', '--configFile',
    help='Path to the JSON test configuration file.',
    required=True
    )
arg_parser.add_argument(
    '-o', '--output',
    help='Path to the directory that the output folder should be placed in.'
    )
arg_parser.add_argument(
    '--bulkPub',
    help='Option for bulk pub tests - place all packages in pub test directory, save time by reusing packages generated for old tests.',
    action="store_true" 
)

args = arg_parser.parse_args(sys.argv[1:])

try:
    with open(args.configFile) as f:
        config = json.load(f)
except:
    print("Not a valid JSON file: {0}".format(args.configFile))
    sys.exit(1)

output = '.'
if args.output:
    if not (os.path.exists(args.output) and os.path.isdir(args.output)):
        print("Not a valid output directory: {0}".format(args.output))
        sys.exit(1)
    output = args.output

if not args.bulkPub:
    output = os.path.join(output, os.path.basename(args.configFile))
    # Remove '.json' from directory name
    output = output[:-5]
    os.mkdir(output)

# Make it an absolute path
output = os.path.abspath(output)

def pkgNameToModuleName(pkgName):
    return pkgName.split("/", 1)[1].capitalize()

def pkgNameToFunctionName(pkgName):
    return pkgName.split("/", 1)[1] + "Print"

for pkgName in config:


    if (not args.bulkPub) or not os.path.isdir(pkgName):
        os.chdir(output)
        os.system('mkdir -p {0}/src'.format(pkgName))
        os.chdir(pkgName)
        os.system('git init')

        for pkgVersion in config[pkgName]:

            # Build manifest JSON file (and collect some string data for the
            # sourcefile later)

            dependencies = {}
            imports = []
            printStmts = []

            for dep in config[pkgName][pkgVersion]:
                dependencies[dep] = {
                "path": os.path.join(output, dep),
                "version": config[pkgName][pkgVersion][dep]
                }
                imports.append("import {0}\n".format(pkgNameToModuleName(dep)))
                printStmts.append("        {0}\n".format(pkgNameToFunctionName(dep)))

            manifest = {
                "name": pkgName,
                "dependencies" : dependencies,
                "sourcedir" : "src",
                "modules" : [ "Main", pkgNameToModuleName(pkgName) ],
                "main" : "Main",
                "executable" : "out"
            }

            with open('ipm.json', 'w+') as f:
                json.dump(manifest, f, sort_keys=True, indent=4)


            # Define idris file to place in the src directory, importing a function from each dependency.

            moduleDef = 'module {0}\n'.format(pkgNameToModuleName(pkgName))

            funcDef = '''

export
{0} : IO ()
{0} = putStrLn \"{1} version {2} ({3})\"
'''.format(
            pkgNameToFunctionName(pkgName),
            pkgName,
            pkgVersion,
            os.path.basename(args.configFile)
        )

        mainStart = '''

main : IO ()
main =
    do  putStrLn "Installed for {0} version {1} ({2}):"
'''.format(
            pkgName,
            pkgVersion,
            os.path.basename(args.configFile)
        )

        allImports = ''.join(imports)
        allPrintStmts = ''.join(printStmts)

        pkgFile = moduleDef + funcDef
        mainFile = "module Main\n" + allImports + mainStart + allPrintStmts

        with open('src/{0}.idr'.format(pkgNameToModuleName(pkgName)), 'w+') as f:
            f.write(pkgFile)

        with open('src/Main.idr', 'w+') as f:
            f.write(mainFile)


        # Commit and tag this version
        os.system('git add . && git commit -m "versionUpgrade"')
        os.system('git tag v{0}'.format(pkgVersion))
