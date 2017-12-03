#!/usr/bin/env python

import sys
import os
import re
import glob
import subprocess
import shutil
import hashlib
import logging
import argparse

log = logging.getLogger()
log.setLevel(logging.INFO)
_handler = logging.StreamHandler()
_handler.setFormatter(logging.Formatter("[%(levelname)s] %(message)s"))
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


def get_hash(input_file, args):
    if not input_file or not os.path.isfile(input_file):
        return None
    args = sorted(args)
    m = hashlib.md5()
    with open(input_file, 'r') as f:
        m.update(f.read())
    for arg in args:
        m.update(arg)
    return m.hexdigest()


def find_in_cache(input_file, args):
    h = get_hash(input_file, args)
    if not h:
        return None
    fname = os.path.join(plot_cache, h)
    if os.path.isfile(fname):
        return fname
    return None


def cache_file(fname, input_file, args):
    if not os.path.isfile(fname):
        return
    h = get_hash(input_file, args)
    if not h:
        return
    out = os.path.join(plot_cache, h)
    shutil.copy(fname, out)


def replace_in_file(fname, pats, out_name=None):
    """Replaces patterns in a given file."""
    if not os.path.isfile(fname):
        log.error("'{0}' does not exist", fname)
        return
    with open(fname, 'r') as f:
        lines = f.readlines()

    def edit_line(line):
        for (guard, pattern, repl) in pats:
            if guard == "" or re.search(guard, line):
                line = re.sub(pattern, repl, line)
        return line
    result = []
    for line in lines:
        result.append(edit_line(line))
    if not out_name:
        out_name = fname
    with open(out_name, 'w') as f:
        f.write("".join(result))


def remove_all(base, name_pat):
    for f in glob.glob(os.path.join(base, name_pat)):
        if os.path.isfile(f):
            os.remove(f)


def remove_files(files):
    for f in files:
        if os.path.isfile(f):
            os.remove(f)


def basename(fname):
    return os.path.splitext(os.path.basename(fname))[0]


# Parse arguments

parser = argparse.ArgumentParser(
    description="Runs FPTaylor with different configurations and plots error model functions.")

parser.add_argument('--debug', action='store_true',
                    help="debug mode")

parser.add_argument('-c', '--config', action='append',
                    help="add a configuration file")

parser.add_argument('-e', '--error', choices=['abs', 'rel', 'ulp'], default='abs',
                    help="error type (overrides error types defined in configuration files)")

parser.add_argument('-r', '--range',
                    help="redefine the range of input variables")

parser.add_argument('-v', '--verbosity', type=int, default=1,
                    help="FPTaylor's verbosity level")

parser.add_argument('-s', '--samples', type=int, default=1000,
                    help="number of sample points (intervals) for plots")

parser.add_argument('--segments', type=int, default=500,
                    help="number of segments for ErrorBounds")

parser.add_argument('--err-samples', type=int, default=10000,
                    help="number of samples for ErrorBounds")

parser.add_argument('--update-cache', action='store_true',
                    help="do not use cached files")

parser.add_argument('input', nargs='+',
                    help="input FPTaylor files")

args = parser.parse_args()

if args.debug:
    log.setLevel(logging.DEBUG)

if not args.config:
    args.config = [None]

log.debug("tmp_dir = {0}".format(plot_tmp))
log.debug("cache_dir = {0}\n".format(plot_cache))

if not os.path.isdir(output_path):
    os.makedirs(output_path)
if not os.path.isdir(plot_tmp):
    os.makedirs(plot_tmp)
if not os.path.isdir(plot_cache):
    os.makedirs(plot_cache)


def restrict_input_vars(fname, range):
    ns = [s.strip() for s in range.split(",")]
    if len(ns) == 1 and ns[0]:
        repl = r"[{0}, \2]".format(ns[0])
    elif len(ns) == 2:
        repl = "["
        repl += ns[0] if ns[0] else r"\1"
        repl += ", "
        repl += ns[1] if ns[1] else r"\2"
        repl += "]"
    else:
        return
    replace_in_file(fname, [(r"[\w]+[\s]+in[\s]*\[.+,.+\]",
                             r"\[(.+),(.+)\]",
                             repl)])


def run_error_bounds(input_file):
    exe_file = os.path.join(plot_tmp, "a.out")
    out_file = os.path.join(plot_tmp, basename(input_file) + "-data.txt")
    remove_files([exe_file, out_file])

    src_files = [os.path.join(error_bounds_path, f) for f in
                 ["search_mpfr.c", "search_mpfr_main.c", "search_mpfr_utils.c"]]

    compile_cmd = ["gcc", "-o", exe_file, "-O3",
                   "-std=c99", "-I" + error_bounds_path]
    compile_cmd += src_files + [input_file]
    compile_cmd += ["-lmpfr", "-lgmp"]

    cmd = [exe_file,
           "-o", out_file,
           "-n", str(args.segments),
           "-s", str(args.err_samples)]

    cached_file = find_in_cache(input_file, cmd)
    if cached_file and not args.update_cache:
        log.info("A cached ErrorBounds result is found")
        shutil.copy(cached_file, out_file)
        return out_file

    run(compile_cmd)
    run(cmd)

    cache_file(out_file, input_file, cmd)
    return out_file


# Run FPTaylor for each input file

fptaylor_extra_args = [
    "-v", str(args.verbosity),
    "--opt-approx", "false",
    "--opt-exact", "true",
    "--tmp-base-dir", fptaylor_tmp,
    "--tmp-date", "false",
    "--log-base-dir", fptaylor_log,
    "--log-append-date", "none"
]

if args.error == 'abs':
    fptaylor_extra_args += ["-abs", "true", "-rel", "false"]
elif args.error == 'rel':
    fptaylor_extra_args += ["-abs", "false", "-rel", "true"]
else:
    # TODO: ulp error
    pass

for fname in args.input:
    if not os.path.isfile(fname):
        log.error("Input file does not exist: {0}".format(fname))
        sys.exit(1)
    shutil.copy(fname, plot_tmp)
    fname = os.path.join(plot_tmp, os.path.basename(fname))
    base_fname = basename(fname) + "-" + args.error
    if args.range:
        restrict_input_vars(fname, args.range)
        base_fname += "-range"
    racket_files = []
    error_bounds_file = None
    remove_all(plot_tmp, base_fname + "*")

    for cfg_file in args.config:
        if not cfg_file:
            # default config
            cfg_name = "default"
            cfg_args = []
        else:
            if not os.path.isfile(cfg_file):
                log.error(
                    "Configuration file does not exist: {0}".format(cfg_file))
                sys.exit(1)
            cfg_name = os.path.splitext(os.path.basename(cfg_file))[0]
            cfg_args = ["-c", cfg_file]

        # FPTaylor
        # TODO: the name should be a pattern such that each task is saved
        # in a separate file

        export_args = []

        if not error_bounds_file:
            error_bounds_file = os.path.join(plot_tmp, base_fname + ".c")
            export_args += ["--export-error-bounds", error_bounds_file]

        racket_file = os.path.join(
            plot_tmp, base_fname + "-" + cfg_name + ".rkt")
        racket_files.append(racket_file)
        export_args += ["--export-racket", racket_file]

        cmd = [fptaylor, fname] + cfg_args + export_args + fptaylor_extra_args
        run(cmd)

        # Adjust the name in the output Racket file
        replace_in_file(racket_file,
                        [("\(define name", '"([^"]*)"', r'"\1-{0}"'.format(cfg_name))])

    # ErrorBounds
    if error_bounds_file:
        data_file = run_error_bounds(error_bounds_file)
    else:
        data_file = None

    # plot-fptaylor.rkt
    image_file = os.path.join(output_path, base_fname + ".png")
    cmd = [racket, racket_plot,
           "--out", image_file,
           "--samples", str(args.samples)]
    if data_file:
        cmd += ["--data", data_file, "--err-type", args.error]
    cmd += racket_files
    run(cmd)
