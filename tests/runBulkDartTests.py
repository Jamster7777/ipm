#!/home/jamie/Documents/uni/diss/ipm/python-venv-ipm/bin/python3.7

import requests, argparse, sys, json, os, time, subprocess, csv

arg_parser = argparse.ArgumentParser(description="Run all generated Dart tests.")

arg_parser.add_argument(
    '-i', '--input',
    help='The directory containing generated tests.',
    required=True
    )

arg_parser.add_argument(
    '--csv',
    help='The output CSV for results',
    required=True
    )

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


def pub_output_to_json(output):
    json = {}
    lines = output.splitlines()
    for l in lines:
        parts = l.split()
        if parts[0] == '+':
            json["pub/" + parts[1]] = parts[2]

    return json

def run_test_for_pkg_man(pkg, ipm=True):
    
    start_time = time.time()
    success = True
    try:
        if ipm:
            result = \
                subprocess \
                    .check_output(
                        'echo \'c,.l\' | sudo -S /home/jamie/Documents/uni/diss/ipm/ipm install --dry-run',
                        shell=True,
                        stderr=subprocess.STDOUT
                        ) \
                    .decode("utf-8")
        else:
            result = subprocess.check_output('pub get --dry-run', shell=True).decode("utf-8")
    except subprocess.CalledProcessError as e:
        success = False
        result = e.output.decode("utf-8")
    
    run_time = time.time() - start_time

    os.system('mkdir -p {0}'.format(os.path.join(args.output, pkg)))

    with open(os.path.join(args.output, pkg, ('pub', 'ipm')[ipm] + '.log'), 'w+') as f:
        f.write(result)

    return success, run_time, result

original_dir = os.getcwd()

pub_path = os.path.join(args.input, 'pub')
ipm_path = os.path.join(args.input, 'ipm')

pkgs = os.listdir(pub_path)

for pkg in pkgs:
    os.chdir(pub_path)
    os.chdir(pkg)
    os.system('rm -f pubspec.lock')
    
    pub_success, pub_run_time, pub_result = run_test_for_pkg_man(pkg, ipm=False)    


    os.chdir(ipm_path)
    os.chdir('pub/')
    os.chdir(pkg)
    
    ipm_success, ipm_run_time, ipm_result = run_test_for_pkg_man(pkg, ipm=True)    

    version = \
        subprocess \
        .check_output('echo \'c,.l\' | sudo -S /home/jamie/Documents/uni/diss/ipm/ipm versions', shell=True) \
        .decode("utf-8") \
        .splitlines()[-2]

    os.chdir(original_dir)

    if pub_success:
        pub_json = pub_output_to_json(pub_result)
        with open(os.path.join(args.output, pkg, 'pub.json'), 'w+') as f:
            json.dump(pub_json, f, sort_keys=True, indent=4)

    if ipm_success:
        ipm_conflict_res_required = 'Conflict resolution required' in ipm_result
        ipm_json = json.loads(ipm_result[ipm_result.index('{'):])

        with open(os.path.join(args.output, pkg, 'ipm.json'), 'w+') as f:
            json.dump(ipm_json, f, sort_keys=True, indent=4)
    else:
        ipm_conflict_res_required = True

    if pub_success and ipm_success:
        
        # ipm outputs the root package version from version solving too, remove this.
        del ipm_json['pub/' + pkg]
        
        equal_sols = json.dumps(pub_json, sort_keys=True, indent=4) == json.dumps(ipm_json, sort_keys=True, indent=4)
    else:
        equal_sols = False

    with open(args.csv, 'a+') as f:
        writer = csv.writer(f, delimiter=',')
        writer.writerow([
            pkg, 
            version,
            ('N', 'Y')[pub_success],
            pub_run_time,
            ('N', 'Y')[ipm_success],
            ipm_run_time,
            ('N', 'Y')[ipm_conflict_res_required],
            ('N', 'Y')[equal_sols]
        ])