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
        help='configuration files for generating tests')
    parser.add_argument('--config-name', default='_tests.cfg',
        help='name of the output configuration file for generated tests')
    parser.add_argument('path',
        help='path to a directory or file for which tests are generated')

    args = parser.parse_args()
    return args

def main():
    args = parse_args()

    files = []
    cfg_path = os.path.join(args.path, args.config_name)
    config = Config(args.config)
    cfg_exported = False
    if os.path.isdir(args.path):
        fnames = glob.glob(os.path.join(args.path, '*.txt'))
    else:
        fnames = [args.path]
    for fname in fnames:
        f = FPTaylorFile(fname)
        if cfg_exported:
            f.generate_tests(config.args)
        else:
            f.generate_tests(config.args, export_options=cfg_path)
            cfg_exported = True
        files.append(f.to_dict())
    data = {}
    if cfg_exported:
        data['config'] = [cfg_path]
    data['files'] = files
    with open('aaa.yml', 'w') as f:
        yaml.dump(data, f, sort_keys=False)

if __name__ == '__main__':
    main()