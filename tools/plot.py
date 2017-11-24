#!/usr/bin/env python

import sys
import os
import glob
import subprocess
import shutil
import logging
import argparse

log = logging.getLogger()
log.setLevel(logging.INFO)
_handler = logging.StreamHandler()
_handler.setFormatter(logging.Formatter("%(levelname)s: %(message)s"))
log.addHandler(_handler)

# Global paths

base_path = os.path.dirname(os.path.normpath(sys.argv[0]))
output_path = os.path.join(base_path, "images")
plot_tmp = os.path.join(base_path, "tmp_plot")
plot_cache = os.path.join(base_path, "cache_plot")

fptaylor_base = os.path.normpath(os.path.join(base_path, ".."))
fptaylor_tmp = os.path.join(base_path, "tmp_fptaylor")
fptaylor_log = os.path.join(base_path, "log_fptaylor")
fptaylor = os.path.join(fptaylor_base, "fptaylor")

error_bounds_path = os.path.normpath(
    os.path.join(base_path, "..", "..", "ErrorBounds"))
racket_plot = os.path.join(error_bounds_path, "racket", "plot-fptaylor.rkt")
racket = "racket"


def run(cmd, ignore_return_codes=[]):
    log.info("Running: {0}".format(" ".join(cmd)))
    ret = subprocess.call(cmd)
    if ret != 0 and ret not in ignore_return_codes:
        log.error("Return code: {0}".format(ret))
        sys.exit(2)
    return ret

# Parse arguments

parser = argparse.ArgumentParser(
    description="Runs FPTaylor with different configurations and plots error model functions.")

parser.add_argument('--debug', action='store_true',
                    help="debug mode")

parser.add_argument('-c', '--config',
                    help="add a configuration file")

parser.add_argument('-v', '--verbosity', type=int, default=1,
                    help="FPTaylor's verbosity level")

parser.add_argument('-s', '--samples', type=int, default=1000,
                    help="Number of sample points (intervals) for plots")

parser.add_argument('input', nargs='+',
                    help="input FPTaylor files")

args = parser.parse_args()

if args.debug:
    log.setLevel(logging.DEBUG)

log.debug("tmp_dir = {0}".format(plot_tmp))
log.debug("cache_dir = {0}\n".format(plot_cache))

if not os.path.isdir(output_path):
    os.makedirs(output_path)
if not os.path.isdir(plot_tmp):
    os.makedirs(plot_tmp)
if not os.path.isdir(plot_cache):
    os.makedirs(plot_cache)

# Run FPTaylor for each input file

fptaylor_extra_args = [
    "-v", str(args.verbosity),
    "--tmp-base-dir", fptaylor_tmp,
    "--tmp-date", "false",
    "--log-base-dir", fptaylor_log,
    "--log-append-date", "none"
]

for fname in args.input:
    if not os.path.isfile(fname):
        log.error("Input file does not exist: {0}".format(fname))
        sys.exit(1)
    base_fname = os.path.basename(fname)
    # TODO: the name should be a pattern such that each task is saved
    # in a separate file
    racket_file = os.path.join(plot_tmp, base_fname + ".rkt")
    cmd = [fptaylor, fname, "--export-racket",
           racket_file] + fptaylor_extra_args
    run(cmd)
    image_file = os.path.join(output_path, base_fname + ".png")
    cmd = [
        racket, racket_plot,
        "--out", image_file,
        "--samples", str(args.samples),
        racket_file
    ]
    run(cmd)
