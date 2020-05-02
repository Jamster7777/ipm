#!/home/jamie/Documents/uni/diss/ipm/python-venv-ipm/bin/python3.7

import requests, argparse, sys, json, os, time, subprocess, csv

arg_parser = argparse.ArgumentParser(description="Retrospective analysis based on observing the log files of failed tests.")

arg_parser.add_argument(
    '-i', '--input',
    help='The directory containing test log files.',
    required=True
    )

arg_parser.add_argument(
    '--csv',
    help='The output CSV for results',
    required=True
    )

args = arg_parser.parse_args(sys.argv[1:])

pkgs = os.listdir(args.input)
original_dir = os.getcwd()

for pkg in pkgs:
    os.chdir(args.input)
    os.chdir(pkg)
    
    with open('pub.log', 'r') as f:
        pub_log = f.read()    

    with open('ipm.log', 'r') as f:
        ipm_log = f.read()
    
    if os.path.exists('ipm.json'):
        ipm_success = True
        with open('ipm.json', 'r') as f:
            ipm_json = json.load(f)
    else:
        ipm_success = False    

    if os.path.exists('pub.json'):
        pub_success = True
        with open('pub.json', 'r') as f:
            pub_json = json.load(f)
    else:
        pub_success = False

    pub_sdk_error = 'requires SDK version' in pub_log
    
    if ipm_success:
        no_deps = len(ipm_json) == 1
    else:
        no_deps = False
    
    os.chdir(original_dir)

    with open(args.csv, 'a+') as f:
        writer = csv.writer(f, delimiter=',')
        writer.writerow([
            ('N', 'Y')[pub_sdk_error],
            ('N', 'Y')[no_deps]
        ])
