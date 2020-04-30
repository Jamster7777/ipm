#!/home/jamie/Documents/uni/diss/ipm/python-venv-ipm/bin/python3.7

import requests, argparse, sys, json, os, time, subprocess

arg_parser = argparse.ArgumentParser(description="Run all generated Dart tests.")

arg_parser.add_argument(
    '-i', '--input',
    help='The directory containing generated tests.',
    required=True
    )

# arg_parser.add_argument(
#     '--csv',
#     help='The output CSV for results',
#     required=True
#     )

arg_parser.add_argument(
    '--output', '-o',
    help='The output folder for lock files',
    required=True
    )

arg_parser.add_argument(
    '-v', '--verbose',
    help='Show debug info',
    action="store_true"
    )

args = arg_parser.parse_args(sys.argv[1:])


def dart_output_to_json(output):
    json = {}
    lines = output.splitlines()
    for l in lines:
        parts = l.split()
        json["pub/" + parts[1]] = parts[2]

    return json

pub_path = os.path.join(args.input, 'pub')
ipm_path = os.path.join(args.input, 'ipm')

os.chdir(pub_path)

for p_dir in os.listdir('.'):
    os.chdir(p_dir)
    
    start_time = time.time()
    pub_result = subprocess.check_output('pub get --dry-run', shell=True)
    pub_time = time.time() - start_time

    os.system('mkdir -p {0}'.format(os.path.join(args.output, p_dir)))

    with open(os.path.join(args.output, p_dir, 'pub.json'), 'w+') as f:
        json.dump(dart_output_to_json(pub_result), f, sort_keys=True, indent=4)
    
