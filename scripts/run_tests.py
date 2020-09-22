import os
import sys
import glob
import argparse
import yaml

import common
from fptaylor import *

log = common.get_log()

def parse_args():
    parser = argparse.ArgumentParser(
        description='Runs FPTaylor tests')
    parser.add_argument('-r', action='store_true',
        help='recursively run tests in all subdirectories')
    parser.add_argument('path',
        help='path to a directory or a test file')

    args = parser.parse_args()
    return args

def main():
    args = parse_args()
    if os.path.isdir(args.path):
        paths = set()
        if args.r:
            test_files = glob.glob(os.path.join(args.path, '**', '*.yml'), recursive=True)
        else:
            test_files = glob.glob(os.path.join(args.path, '*.yml'))
        test_files.sort()
    else:
        test_files = [args.path]

    passed, failed = 0, 0
    for test_file in test_files:
        with open(test_file, 'r') as f:
            tests = yaml.safe_load(f)

        base_path = os.path.dirname(test_file)
        files = [FPTaylorFile(f, base_path=base_path) for f in tests['files']]
        config = Config(tests.get('config', []), base_path=base_path)
        for f in files:
            f_passed, f_failed = f.run_tests(config.args)
            passed += f_passed
            failed += f_failed
    
    print()
    print(f'Passed: {passed} / {passed + failed}', flush=True)
    if failed:
        sys.exit(1)


if __name__ == '__main__':
    main()