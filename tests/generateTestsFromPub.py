import requests, argparse, sys

arg_parser = argparse.ArgumentParser(description="Convert a dart package dependency tree into a test config file for testing ipm with.")

arg_parser.add_argument(
    '-p', '--pkg',
    help='Name of the dart package (from https://pub.dev/dart/packages)',
    required=True
    )

arg_parser.add_argument(
    '-u', '--username',
    help='Username to use for test packages (dart does not include a group/user name in its package names.',
    required=True
    )

args = arg_parser.parse_args(sys.argv[1:])

packages_added = set()

def convert_to_ipm_name(name):
    return '{0}/{1}'.format(args.username, name)

def add_package(package, is_root=False):
    
    if not (package in packages_added):
        
        packages_added.add(package)
        response = requests.get('https://pub.dartlang.org/api/packages/{0}'.format(package)).json()
        ipm_name = convert_to_ipm_name(package)
        output[ipm_name] = {}
        deps_to_fetch = set()
        versions = []
        
        if is_root:
            versions.append(response['latest'])
        else:
            for v in response['versions']:
                versions.append(v)
        
        for vObj in versions:
            version = vObj['version']
            deps = vObj['pubspec']['dependencies']
            depsIpmNames = dict((convert_to_ipm_name(k), v) for k, v in deps.items())
            output[ipm_name][version] = depsIpmNames
            deps_to_fetch.union(deps.keys())


output = {}
add_package(args.pkg, is_root=True)