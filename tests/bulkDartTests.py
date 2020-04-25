#!/home/jamie/Documents/uni/diss/ipm/python-venv-ipm/bin/python3.7

import requests, argparse, sys, json, os, timeit

arg_parser = argparse.ArgumentParser(description="Convert a dart package dependency tree into a test config file for testing ipm with.")

arg_parser.add_argument(
    '-n', '--testNo',
    help='Test number to identify the first generated test with.',
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

args = arg_parser.parse_args(sys.argv[1:])

response_json = requests.get('https://pub.dartlang.org/api/packages/').json()


# TODO remove
package_names = [ 'implicitly_animated_reorderable_list' ]

# for p in response_json['packages']:
#     if p['name']:
#         package_names.append(p['name'])

test_no = 200

wd = os.getcwd()


for p in package_names:
    os.chdir(wd)
    os.system('./generateTestsFromDart.py -p {0} -n {1} -o bulkDart/'.format(p, test_no))
    
    test_name = test_no + '-' + p
    test_dir = '/media/HDD/fedora/diss-testing/ipm/' + test_name

    os.system('./generateTestRepos.py -c bulkDart/{0}.json -o {1}'.format(test_name, test_dir))
    os.chdir(test_dir)
    
    ipm_time = timeit.timeit(os.system('ipm install --dry-run'))

    

    test_no = test_no + 1

def dart_output_to_json(output):
    json = {}
    lines = output.splitlines()
    for l in lines:
        parts = l.split()
        json["u11/" + parts[1]] = parts[2]

    return json


# print(json.dumps(dart_output_to_json(str), sort_keys=True, indent=4))
