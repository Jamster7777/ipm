#!/home/jamie/Documents/uni/diss/ipm/python-venv-ipm/bin/python3.7

import requests, argparse, sys, json, os, timeit

arg_parser = argparse.ArgumentParser(description="Generate several Dart tests based on the most recently published Dart packages.")

arg_parser.add_argument(
    '-l', '--limit',
    help='Number of tests to generate',
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

response_json = requests.get('https://pub.dartlang.org/api/packages?sort=popularity').json()

pub_path = os.path.join(args.output, 'pub')
ipm_path = os.path.join(args.output, 'ipm')

os.system('mkdir -p {0}'.format(pub_path))
os.system('mkdir -p {0}'.format(ipm_path))

limit = int(args.limit)
no_generated = 0

for p in response_json['packages']:

    if no_generated >= limit:
        break

    try:
        p['latest']['pubspec']['dependencies']['flutter']
    except:
        if p['name'] and p['latest']['pubspec'] and p['latest']['pubspec']['homepage'] and ("github.com/" in p['latest']['pubspec']['homepage']):
            
            if args.verbose:
                print(p['name'] + ' has a GitHub url')

            check_github = requests.get(p['latest']['pubspec']['homepage'])

            if check_github.status_code < 300:
                
                if args.verbose:
                    print(p['name'] + ' has a PUBLIC GitHub url')

                # Can only test packages with a GitHub url we can clone from, to compare against Dart

                p_pub_path = os.path.join(pub_path, p['name'])
                os.system('git clone {0} {1}'.format(p['latest']['pubspec']['homepage'], p_pub_path))
                
                if os.path.isfile(os.path.join(p_pub_path, 'pubspec.yaml')):
                    
                    if args.verbose:
                        print(p['name'] + ' has a pubspec.yaml file in the parent directory')
                        print('Generating ipm test config for this package...')

                    # There is a pubspec file we can test, so we'll generate an ipm test.
                    
                    os.system('./generateTestsFromDart.py -p {0} -o {1} --bulkPub'.format(p['name'], ipm_path))

                    if args.verbose:
                        print('Generating test git repos (that do not already exist)')

                    os.system('./generateTestRepos.py -c {0} -o {1} --bulkPub'.format(os.path.join(ipm_path , p['name'] + '.json'), ipm_path))
                    no_generated = no_generated + 1
        
