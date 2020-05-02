#!/home/jamie/Documents/uni/diss/ipm/python-venv-ipm/bin/python3.7

import requests, argparse, sys, json, os, yaml, subprocess

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

pub_path = os.path.join(args.output, 'pub')
ipm_path = os.path.join(args.output, 'ipm')

os.system('mkdir -p {0}'.format(pub_path))
os.system('mkdir -p {0}'.format(ipm_path))

limit = int(args.limit)
no_generated = 0
page_no = 0

while (no_generated < limit):

    page_no = page_no + 1
    response_json = requests.get('https://pub.dartlang.org/api/packages?page={0}'.format(page_no)).json()

    for p in response_json['packages']:

        if no_generated >= limit:
            break

        p_pub_path = os.path.join(pub_path, p['name'])

        # Skip packages which have already had tests generated for them
        if not os.path.isdir(p_pub_path):

            # Flutter packages use a different version of pub
            try:
                p['latest']['pubspec']['dependencies']['flutter']
            except:
                if p['name'] and p['latest']['pubspec'] and p['latest']['pubspec']['homepage'] and ("github.com/" in p['latest']['pubspec']['homepage']):

                    if args.verbose:
                        print(p['name'] + ' has a GitHub url')

                    # Check the git repo is public to prevent a password prompt
                    if requests.get(p['latest']['pubspec']['homepage']).status_code < 300:
                        try:
                            subprocess \
                                .check_output('git clone {0} {1}'.format(p['latest']['pubspec']['homepage'], p_pub_path), shell=True)
                        except subprocess.CalledProcessError:
                            continue

                        if args.verbose:
                            print(p['name'] + ' has a cloneable GitHub url')

                        if os.path.isfile(os.path.join(p_pub_path, 'pubspec.yaml')):

                            with open(os.path.join(p_pub_path, 'pubspec.yaml'), 'r') as f:
                                yamlObj = yaml.load(f)

                            try:
                                del yamlObj['dev_dependencies']
                                with open(os.path.join(p_pub_path, 'pubspec.yaml'), 'w+') as f:
                                    f.write(yaml.dump(yamlObj))
                            except:
                                # Ignore
                                print('No dev_dependencies to delete.')

                            if args.verbose:
                                print(p['name'] + ' has a pubspec.yaml file in the parent directory')
                                print('Generating ipm test config for this package...')

                            # There is a pubspec file we can test, so we'll generate an ipm test.

                            os.system('./generateTestsFromDart.py -p {0} -o {1} --bulkPub'.format(p['name'], ipm_path))

                            if args.verbose:
                                print('Generating test git repos (that do not already exist)')

                            os.system('./generateTestRepos.py -c {0} -o {1} --bulkPub'.format(os.path.join(ipm_path , p['name'] + '.json'), ipm_path))
                            no_generated = no_generated + 1
                        else:
                            os.system('rm -rf {0}'.format(p_pub_path))
            else:
                if args.verbose:
                    print(p['name'] + ' has already had a test generated for it, skipping.')
