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
    args = parser.parse_args()
    return args

def main():
    args = parse_args()
    with open('aaa.yml', 'r') as f:
        tests = yaml.safe_load(f)

    files = [FPTaylorFile(f) for f in tests['files']]
    config = Config(tests.get('config', []))
    for f in files:
        f.run_tests(config.args)

if __name__ == '__main__':
    main()