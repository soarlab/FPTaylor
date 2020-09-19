#!/usr/bin/env python

import sys
import os
import re
import glob
import shutil
import argparse
from fractions import *

import common

log = common.get_log()

# Global paths

base_path = os.path.dirname(os.path.normpath(sys.argv[0]))
tmp_base_path = os.path.join(base_path, "tmp")
tmp_path = os.path.join(tmp_base_path, "tmp_gappa_data")
cache_path = os.path.join(tmp_base_path, "cache_gappa_data")

fptaylor_base = os.path.normpath(os.path.join(base_path, ".."))
fptaylor_tmp = os.path.join(tmp_base_path, "tmp_fptaylor")
fptaylor_log = os.path.join(tmp_base_path, "log_export_fptaylor")
fptaylor = os.path.join(fptaylor_base, "fptaylor")

gappa = os.path.expanduser(os.path.normpath("~/Work/tools/gappa-1.3.1/src/gappa"))

fpbench_path = os.path.normpath(
    os.path.join(base_path, "..", "..", "forks", "FPBench", "tools"))
core2gappa = os.path.join(fpbench_path, "core2gappa.rkt")
racket = "racket"


def basename(fname):
    return os.path.splitext(os.path.basename(fname))[0]


# Parse arguments

parser = argparse.ArgumentParser(
    description="Splits input intervals into small pieces and runs Gappa on each subinterval")

parser.add_argument('--debug', action='store_true',
                    help="debug mode")

parser.add_argument('-e', '--error', choices=['abs', 'rel'], default='abs',
                    help="error type")

parser.add_argument('-t', '--type', default='64',
                    choices=['16', '32', '64', 'real'], 
                    help="default type of variables and rounding operations")

parser.add_argument('-v', '--verbosity', type=int, default=0,
                    help="FPTaylor's verbosity level")

parser.add_argument('-o', '--output-path', default=".",
                    help="specifies where to save data files")

parser.add_argument('-n', '--segments', type=int, default=100,
                    help="number of subintervals")

parser.add_argument('input',
                    help="input FPTaylor file")

args = parser.parse_args()

if args.debug:
    log.setLevel(logging.DEBUG)

if not os.path.isdir(tmp_path):
    os.makedirs(tmp_path)
if not os.path.isdir(cache_path):
    os.makedirs(cache_path)

common.remove_all(tmp_path, "*")


def decode_binary(s):
    pat = r'([0-9+-]+)(b([0-9+-]+))?'
    m = re.match(pat, s)
    v = Fraction(m.group(1))
    if m.group(3):
        p = Fraction(2) ** int(m.group(3))
        v *= p
    return v

def get_input_bounds(input_file):
    pat = r'in[\s]*\[([0-9.e+-]+)[\s]*,[\s]*([0-9.e+-]+)\]'
    with open(input_file, 'r') as f:
        data = f.read()
    m = re.search(pat, data)
    return float(m.group(1)), float(m.group(2))

class GappaTask:
    def __init__(self, name):
        self.input_files = []
        self.name = name

    def __repr__(self):
        s = "GappaTask({0}): {1}".format(self.name, self.input_files)
        return s

    def create_data(self, out_file):
        pat = r'in[\s]*\[([0-9b+-]+)[\s]*(\{[^}]*\})?,[\s]*([0-9b+-]+)'
        self.input_files.sort(key=lambda x: x[0])
        data = []
        log.info("Running Gappa...")
        total = len(self.input_files)
        i = 0
        for _, input_file in self.input_files:
            bounds = get_input_bounds(input_file)
            cmd = [gappa, input_file]
            output = common.run_output(cmd, silent=True)
            m = re.search(pat, output)
            v1 = decode_binary(m.group(1))
            v2 = decode_binary(m.group(3))
            v = max(abs(v1), abs(v2))
            data.append((bounds, v))
            i += 1
            sys.stdout.write("\r{0}/{1}       ".format(i, total))
        print("")
        log.info("done")
        with open(out_file, 'w') as f:
            n = len(data)
            if n != args.segments:
                log.error("Wrong number of results: {0} (expected {1})".format(n, args.segments))
                lo = 1
                hi = 2
            else:
                lo = data[0][0][0]
                hi = data[n - 1][0][1]
            f.write("[Gappa]{0}\n".format(self.name))
            i = 1
            for ((low, high), v) in data:
                if args.error == 'abs':
                    abs_err = float(v)
                    rel_err = 0
                else:
                    abs_err = 0
                    rel_err = float(v)
                if i == 1:
                    low = lo
                if i == n:
                    high = hi
                f.write("{0}, {1}, {2}, {3}, {4}, 0\n".format(i, low, high, abs_err, rel_err))
                i += 1


# Export FPTaylor tasks to FPCore

out_file = os.path.join(tmp_path, basename(args.input) + ".fpcore")

cmd = [fptaylor, args.input, 
       "--fpcore-out", out_file,
       "--log-base-dir", fptaylor_log,
       "--log-append-date", "none",
       "--tmp-base-dir", fptaylor_tmp,
       "--tmp-date", "false",
       "-v", str(args.verbosity)]

rnd_types = {
    "16": ("float16", "rnd16", "binary16"),
    "32": ("float32", "rnd32", "binary32"),
    "64": ("float64", "rnd64", "binary64"),
    "real": ("real", "rnd64", "real")
}
var_type, rnd_type, fpcore_type = rnd_types[args.type]
cmd += ["--default-var-type", var_type]
cmd += ["--default-rnd", rnd_type]

common.run(cmd, log=log)


# Run core2gappa.rkt

cmd = [racket, core2gappa,
       "--var-precision", fpcore_type,
       "--split", str(args.segments)]

if args.error == 'rel':
    cmd += ["--rel-error"]

cmd += ["--out-path", tmp_path]

common.run(cmd + ["--", out_file], log=log)


# Collect files corresponding to each task

gappa_tasks = dict()

for file_path in glob.glob(os.path.join(tmp_path, "*.g")):
    pat = r'(.+)\_case([0-9]+)\.g'
    fname = os.path.basename(file_path)
    m = re.match(pat, fname)
    if not m:
        task_name = os.path.splitext(fname)[0]
        case = 0
    else:
        task_name = m.group(1)
        case = int(m.group(2))
    if task_name not in gappa_tasks:
        gappa_tasks[task_name] = GappaTask(task_name)
    gappa_tasks[task_name].input_files.append((case, file_path))

for task_name, task in gappa_tasks.iteritems():
    out_file = os.path.join(args.output_path, "gappa-data-" + task_name + ".txt")
    task.create_data(out_file)