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
        description='Generates FPTaylor tests')
    parser.add_argument('--config', nargs='+', default=[],
        help='configuration files (and options) for generating tests')
    parser.add_argument('--config-name', default='_tests.cfg',
        help='name of the output configuration file for generated tests')
    parser.add_argument('--test-file', default='_tests.yml',
        help='name of the output test file')
    parser.add_argument('path',
        help='path to a directory or file for which tests are generated')

    args = parser.parse_args()
    return args

def main():
    args = parse_args()

    if os.path.isdir(args.path):
        base_path = args.path
        fnames = sorted(glob.glob(os.path.join(args.path, '*.txt')))
    else:
        base_path = os.path.dirname(args.path)
        fnames = [args.path]
    files = []
    cfg_path = os.path.join(base_path, args.config_name)
    config = Config(args.config)
    cfg_exported = False
    for fname in fnames:
        f = FPTaylorFile(fname)
        if cfg_exported:
            f.generate_tests(config.args)
        else:
            f.generate_tests(config.args, export_options=cfg_path)
            cfg_exported = True
        f_data = f.to_dict()
        f_data['name'] = os.path.relpath(f_data['name'], base_path)
        files.append(f_data)
    data = {}
    if cfg_exported:
        # The saved configuration file path is relative to the base path
        data['config'] = [args.config_name]
    data['files'] = files
    out_path = os.path.join(base_path, args.test_file)
    with open(out_path, 'w') as f:
        yaml.dump(data, f, sort_keys=False)

if __name__ == '__main__':
    main()