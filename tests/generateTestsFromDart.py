#!/home/jamie/Documents/uni/diss/ipm/python-venv-ipm/bin/python3.7

import requests, argparse, sys, json, os

arg_parser = argparse.ArgumentParser(description="Convert a dart package dependency tree into a test config file for testing ipm with.")

arg_parser.add_argument(
    '-p', '--pkg',
    help='Name of the dart package (from https://pub.dev/dart/packages)',
    required=True
    )

arg_parser.add_argument(
    '-o', '--output',
    help='The output folder to place the config file in.',
    required=True
    )

arg_parser.add_argument(
    '-v', '--verbose',
    help='Show debug info',
    action="store_true"
    )

arg_parser.add_argument(
    '--bulkPub',
    help='Option for bulk pub tests - place all packages in pub test directory, save time by reusing packages generated for old tests.',
    action="store_true" 
)

args = arg_parser.parse_args(sys.argv[1:])

packages_added = set()

def convert_to_ipm_name(name):
    return 'pub/{0}'.format(name)

def merge_keys(o1, o2):
    merged = o1
    for k, v in o2.items():
        if not k in o1:
            merged[k] = v
    return merged

def add_package(package, is_root=False):

    if not (package in packages_added):

        packages_added.add(package)

        response = requests.get('https://pub.dartlang.org/api/packages/{0}'.format(package))

        if args.verbose:
            print("Response from API for package '{0}':\n{1}".format(package, response))
            print ("Fetched config for {0}".format(package))


        ipm_name = convert_to_ipm_name(package)
        output[ipm_name] = {}

        if response.status_code >= 400:
            return

        response_json = response.json()
        deps_to_fetch = set()
        versions = []

        if is_root:
            versions.append(response_json['latest'])
        else:
            for v in response_json['versions']:
                versions.append(v)

        for vObj in versions:
            if args.verbose:
                print(json.dumps(vObj, indent=4))
            version = vObj['version']

            try:
                deps = vObj['pubspec']['dependencies']
                if deps is None:
                    deps = {}
            except (KeyError, AttributeError):
                deps = {}

            # if is_root:
            #     try:
            #         dev_deps = vObj['pubspec']['dev_dependencies']
            #         if dev_deps is None:
            #             dev_deps = {}
            #     except (KeyError, AttributeError):
            #         dev_deps = {}

            #     all_deps = merge_keys(deps, dev_deps)
            # else:
            #     all_deps = deps
            
            # TODO
            all_deps = deps

            depsIpmNames = dict((convert_to_ipm_name(k), v) for k, v in all_deps.items())
            output[ipm_name][version] = depsIpmNames
            deps_to_fetch |= set(all_deps.keys())

        if args.verbose:
            print("Deps to fetch:\n{0}".format(deps_to_fetch))

        for dep in deps_to_fetch:
            if (not args.bulkPub) or (not os.path.isdir(os.path.join(args.output, package))):
                add_package(dep)

output = {}
add_package(args.pkg, is_root=True)

with open(os.path.join(args.output, "{0}.json".format(args.pkg)), 'w+') as f:
    json.dump(output, f, sort_keys=True, indent=4)
