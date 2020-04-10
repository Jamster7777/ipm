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
        if is_root:
            version = response['latest']['version']
            deps = response['latest']['pubspec']['dependencies']
            output[ipm_name][version] = deps
            deps_to_fetch.union(deps.keys())
        else:
            for v in response['versions']:
                version = v['version']
                deps = v['pubspec']['dependencies']
                output[ipm_name][version] = deps
                deps_to_fetch.union(deps.keys())
                

output = {}
add_package(args.pkg, is_root=True)