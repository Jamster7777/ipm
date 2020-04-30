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
    help='The output folder to place tests in',
    required=True
    )

arg_parser.add_argument(
    '-v', '--verbose',
    help='Show debug info',
    action="store_true"
    )

args = arg_parser.parse_args(sys.argv[1:])

response_json = requests.get('https://pub.dartlang.org/api/packages/').json()


# # TODO remove
# package_names = [ 'implicitly_animated_reorderable_list' ]

pub_path = os.path.join(args.output, 'pub')
ipm_path = os.path.join(args.output, 'ipm')

os.system('mkdir -p {0}'.format(pub_path))
os.system('mkdir -p {0}'.format(ipm_path))

def clone_repo(test_dir, url):
    
    return 

test_no = 200

for p in response_json['packages']:
    
    if p['name'] and p['pubspec']['homepage'] and ("github.com/" in p['pubspec']['homepage']):
        
        # Can only test packages with a GitHub url we can clone from, to compare against Dart

        p_folder = '{0}-{1}'.format(test_no, p['name'])
        p_pub_path = os.path.join(pub_path, p_folder)
        os.system('git clone {0} {1}'.format(p['pubspec']['homepage'], p_pub_path))
        
        if os.path.isfile(os.path.join(p_pub_path, 'pubspec.yaml')):
            
            # There is a pubspec file we can test, so we'll generate an ipm test.
            
            

    test_no = test_no + 1




# wd = os.getcwd()


# def ipm_test(p):
#     os.chdir(wd)
#     os.system('./generateTestsFromDart.py -p {0} -n {1} -o bulkDart/'.format(p, test_no))
    
#     test_dir = '/media/HDD/fedora/diss-testing/ipm/' + test_name

#     os.system('./generateTestRepos.py -c bulkDart/{0}.json -o {1}'.format(test_name, test_dir))
#     os.chdir(test_dir)
    
#     return timeit.timeit(os.system('ipm install --dry-run'))

# def pub_test(p):
#     test_dir = '/media/HDD/fedora/diss-testing/pub' + test_name
#     os.system('mkdir -p {0}'.format(test_dir))
#     os.chdir(test_dir)

# for p in package_names:
#     test_name = test_no + '-' + p
#     ipm_time = ipm_test(p)
#     pub_time = pub_test(p)
#     test_no = test_no + 1

# def dart_output_to_json(output):
#     json = {}
#     lines = output.splitlines()
#     for l in lines:
#         parts = l.split()
#         json["u11/" + parts[1]] = parts[2]

#     return json


# print(json.dumps(dart_output_to_json(str), sort_keys=True, indent=4))
