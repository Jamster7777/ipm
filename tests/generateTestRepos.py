#!/usr/bin/python

import argparse, sys, os, json

# Argument parsing is inspired by the relevant answer here:
# https://stackoverflow.com/questions/14360389/getting-file-path-from-command-line-argument-in-python

arg_parser = argparse.ArgumentParser(description="Generate a set of git reps for testing the Idris package manager on.")
arg_parser.add_argument(
    '-c', '--configFile',
    help='Path to the JSON test configuration file.',
    required=True
    )
arg_parser.add_argument(
    '-o', '--output', 
    help='Path to the directory that the output folder should be placed in.'
    )

args = arg_parser.parse_args(sys.argv[1:])

try:
    with open(args.configFile) as f:
        config = json.load(f)
except:
    print("Not a valid JSON file: {0}".format(args.configFile))
    sys.exit(1)

output = '.'
if args.output:
    if not (os.path.exists(args.output) and os.path.isdir()):
        print("Not a valid output directory: {0}".format(args.outputD))
        sys.exit(1)
    output = args.output

