#!/usr/bin/env python3

import sys
import os
import re
import glob
import shutil
import argparse
import logging

sys.path.append('..')
import common

log = common.get_log()

# Global paths

base_path = os.path.normpath(os.path.dirname(os.path.normpath(sys.argv[0])))
output_path = os.path.join(base_path, "images")
tmp_path = os.path.join(base_path, "tmp")

plot_tmp = os.path.join(tmp_path, "tmp_plot")
plot_cache = os.path.join(tmp_path, "cache_plot")

fptaylor_base = os.path.normpath(os.path.join(base_path, "..", ".."))
fptaylor_tmp = os.path.join(tmp_path, "tmp_fptaylor")
fptaylor_log = os.path.join(tmp_path, "log_fptaylor")
fptaylor = os.path.join(fptaylor_base, "fptaylor")

error_bounds_path = os.path.normpath(
    os.path.join(base_path, "..", "..", "..", "..", "ErrorBounds"))
racket_plot = os.path.join(error_bounds_path, "racket", "plot-data.rkt")

fpbench_path = os.path.normpath(
    os.path.join(base_path, "..", "..", "..", "FPBench", "src"))
core2fptaylor = os.path.join(fpbench_path, "core2fptaylor.rkt")

racket = "racket"
gnuplot = "gnuplot"


def files_from_template(fname_template):
    result = {}
    if not fname_template:
        return result
    pat = re.sub(r"\\{task\\}", r"(.*)", re.escape(fname_template))
    log.debug("pat = {0}".format(pat))
    for fname in glob.glob(re.sub("{task}", "*", fname_template)):
        if os.path.isfile(fname):
            log.debug("fname = {0}".format(fname))
            m = re.match(pat, fname)
            task = m.group(1)
            result[task] = fname
    return result

# Parse arguments

parser = argparse.ArgumentParser(
    description="Runs FPTaylor with different configurations and plots error model functions.")

parser.add_argument('--debug', action='store_true',
                    help="debug mode")

parser.add_argument('-c', '--config', action='append', nargs='+',
                    help="add a configuration file (or several files)")

parser.add_argument('-e', '--error', choices=['abs', 'rel', 'ulp'], default='abs',
                    help="error type (overrides error types defined in configuration files)")

parser.add_argument('-t', '--type', default='64',
                    choices=['16', '32', '64', 'real'], 
                    help="default type of variables and rounding operations.\
                          Also controls flags of ErrorBounds.")

parser.add_argument('-r', '--range',
                    help="redefine the range of input variables")

parser.add_argument('-v', '--verbosity', type=int, default=1,
                    help="FPTaylor's verbosity level")

parser.add_argument('-s', '--samples', type=int, default=1000,
                    help="number of sample points (intervals) for plots")

parser.add_argument('--mpfr-prec', type=int,
                    help="MPFR precision in ErrorBounds")

parser.add_argument('--mpfi', action='store_true',
                    help="use MPFI in ErrorBounds")

parser.add_argument('--show-extra-errors', action='store_true',
                    help="explicitly plot all extra error terms (total2, etc.)")

parser.add_argument('--subexprs', action='store_true',
                    help="produce plots for all subexpressions")

parser.add_argument('--gappa', action='store_true',
                    help="produce Gappa plots")

parser.add_argument('--gappa-segments', type=int, default=200,
                    help="number of subintervals for Gappa plots")

parser.add_argument('--segments', type=int, default=500,
                    help="number of segments for ErrorBounds")

parser.add_argument('--err-samples', type=int, default=10000,
                    help="number of samples for ErrorBounds")

parser.add_argument('--adaptive', action='store_true',
                    help="produce model data with an adaptive algorithm")

parser.add_argument('--data-plot-style', choices=['stack', 'lines'],
                    default='stack',
                    help="specifies how to plot ErrorBounds results")

parser.add_argument('--width', type=int,
                    help="plot width")

parser.add_argument('--height', type=int,
                    help="plot height")

parser.add_argument('--gnuplot', choices=['png', 'html'],
                    help="produce plots with gnuplot")

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

common.remove_all(plot_tmp, "*")
common.remove_all(fptaylor_tmp, "*")


def basename(fname):
    return os.path.splitext(os.path.basename(fname))[0]


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
    common.replace_in_file(fname, [(r"[\w]+[\s]+in[\s]*\[.+,.+\]",
                                    r"\[(.+),(.+)\]",
                                    repl)])


def run_error_bounds(input_file):
    exe_file = os.path.join(plot_tmp, "a.out")
    out_file = os.path.join(plot_tmp, basename(input_file) + "-data.txt")
    common.remove_files([exe_file, out_file])

    src_files = ["search_mp_main.c"]
    if args.mpfi:
        src_files += ["search_mpfi.c"]
    else:
        src_files += ["search_mpfr.c"]
    src_files = [os.path.join(error_bounds_path, f) for f in src_files]

    compile_cmd = ["gcc", "-o", exe_file, "-O3",
                   "-std=c99", "-I" + error_bounds_path]
    compile_cmd += src_files + [input_file]
    if args.mpfi:
        compile_cmd += ["-DUSE_MPFI", "-lmpfi"]
    compile_cmd += ["-lmpfr", "-lgmp"]

    cmd_args = ["-n", str(args.segments),
                "-s", str(args.err_samples)]

    if args.type == "32":
        cmd_args += ["-f"]
    elif args.type == "real":
        cmd_args += ["-r"]
    
    if args.mpfr_prec:
        cmd_args += ["-p", str(args.mpfr_prec)]

    cache_args = list(cmd_args)
    if args.mpfi:
        cache_args += "mpfi"

    cached_file = common.find_in_cache(plot_cache, input_file, cache_args)
    if cached_file and not args.update_cache:
        log.info("A cached ErrorBounds result is found")
        shutil.copy(cached_file, out_file)
        return out_file

    common.run(compile_cmd, log=log)
    common.run([exe_file] + cmd_args + ["-o", out_file], log=log)

    common.cache_file(plot_cache, out_file, input_file, cache_args)
    return out_file


def run_data_mpfi(input_file):
    exe_file = os.path.join(plot_tmp, "a.out")
    out_file = os.path.join(plot_tmp, basename(input_file) + "-model-data.txt")
    common.remove_files([exe_file, out_file])

    src_files = ["data_mpfi.c", "func.c", "data_mpfi_main.c"]
    src_files = [os.path.join(error_bounds_path, f) for f in src_files]

    compile_cmd = ["gcc", "-o", exe_file, "-O3",
                   "-std=c99", "-I" + error_bounds_path]
    compile_cmd += src_files + [input_file]
    compile_cmd += ["-lmpfi", "-lmpfr", "-lgmp"]

    cmd_args = ["-n", str(args.samples)]
    if args.mpfr_prec:
        cmd_args += ["-p", str(args.mpfr_prec)]
    if args.adaptive:
        cmd_args += ["-a"]

    common.run(compile_cmd, log=log)
    common.run([exe_file] + cmd_args + ["-o", out_file], log=log)
    return out_file


# Data files

def decode_line(line):
    pat = r'(\[([0-9.e+-]+),([0-9.e+-]+)\])|([0-9.e+-]+)'
    result = []
    for m in re.finditer(pat, line):
        if m.group(4):
            result.append(float(m.group(4)))
        else:
            result.append((float(m.group(2)), float(m.group(3))))
    return result


class DataSet:
    def __init__(self, title, data):
        self.title = title
        self.data = data

    def __repr__(self):
        s = "DataSet({0}):\n".format(self.title)
        for row in self.data:
            s += "{0}\n".format(row)
        return s

    def bounds(self, col):
        y_min = 1e+308
        y_max = -1e+308
        for row in self.data:
            y = row[col]
            if isinstance(y, tuple):
                y_min = min(y[0], y_min)
                y_max = max(y[1], y_max)
            else:
                y_min = min(y, y_min)
                y_max = max(y, y_max)
        return (y_min, y_max)

    def export_for_gnuplot(self, out_file, cols, lines_flag):
        out_file.write(re.sub(r'_', r'\\\\_', self.title))
        out_file.write("\n")
        vals = None
        for row in self.data:
            vals = []
            for col in cols:
                x = row[col]
                if isinstance(x, tuple):
                    vals += [x[0], x[1]]
                else:
                    vals.append(x)
            out_file.write(" ".join(["{0}".format(x) for x in vals]))
            out_file.write("\n")
        if lines_flag and vals:
            vals[0] = vals[1]
            out_file.write(" ".join(["{0}".format(x) for x in vals]))
            out_file.write("\n")


class DataFile:
    def __init__(self, fname):
        self.data_sets = []
        with open(fname, 'r') as f:
            k = 2
            data = []
            title = ""
            for line in f:
                line = line.strip('\n')
                if line.startswith("#"):
                    continue
                if not line:
                    k += 1
                    continue
                if k < 2:
                    data.append(decode_line(line))
                    continue
                if title or data:
                    self.data_sets.append(DataSet(title, data))
                title = line
                data = []
                k = 0
            if title or data:
                self.data_sets.append(DataSet(title, data))

    def __repr__(self):
        s = ""
        for d in self.data_sets:
            s += str(d)
        return s

    def __getitem__(self, index):
        return self.data_sets[index]

    def count(self):
        return len(self.data_sets)

    def bounds(self, col):
        y_min, y_max = 1e+308, -1e-308
        for data in self.data_sets:
            a, b = data.bounds(col)
            y_min = min(a, y_min)
            y_max = max(b, y_max)
        return (y_min, y_max)

    def export_for_gnuplot(self, out_fname, cols, lines_flag=False):
        with open(out_fname, 'w') as f:
            n = self.count()
            for data in self.data_sets:
                data.export_for_gnuplot(f, cols, lines_flag)
                n -= 1
                if n > 0:
                    f.write("\n\n")

# Tasks

class PlotTask:
    def __init__(self, input_name, base_name):
        self.input_name = input_name
        self.base_name = base_name
        self.model_files = []
        self.error_files = []
        self.title = None

    def add_error_file(self, fname, style=None):
        self.error_files.append((fname, style))

    def add_model_file(self, fname, style=None):
        self.model_files.append((fname, style))

    def create_gnuplot_file(self, out_file):
        script_file = os.path.join(plot_tmp, self.base_name + ".gnuplot")
        if args.width:
            width = int(args.width)
        else:
            width = 800
        if args.height:
            height = int(args.height)
        else:
            height = 600
        with open(script_file, 'w') as f:
            # terminal
            if args.gnuplot == "html":
                f.write("set terminal canvas standalone mousing size {0},{1}\n".format(
                    width, height))
            else:
                f.write("set terminal png size {0},{1}\n".format(width, height))
            f.write("set output '{0}'\n".format(out_file))
            
            # parameters
            f.write("set style fill solid\n")
            if self.title:
                f.write("set title '{0}'\n".format(self.title))
            
            # plots
            plot_cmds = []
            y_max = -1e+308

            # error files
            for (error_file, style) in self.error_files:
                if not style:
                    style = args.data_plot_style
                lines_flag = (style == 'lines')

                col = 3
                if args.error == 'abs':
                    col = 3
                elif args.error == 'rel':
                    col = 4
                elif args.error == 'ulp':
                    col = 5

                data = DataFile(error_file)
                y_min_data, y_max_data = data.bounds(col)
                y_max = max(y_max_data, y_max)

                gnuplot_file = os.path.join(os.path.dirname(error_file),
                                            "[gnuplot]" + os.path.basename(error_file))
                data.export_for_gnuplot(gnuplot_file, [1,2,col], lines_flag=lines_flag)

                if not lines_flag:
                    plot_cmds.append("  '{0}' using {1} with {2} title columnheader(1)".format(
                        gnuplot_file,
                        "1:2:1:2:(0):3",
                        "boxxyerrorbars"
                    ))
                    if args.mpfi:
                        plot_cmds.append("  '{0}' using {1} with {2} title '{3}'".format(
                            gnuplot_file,
                            "1:2:1:2:3:4",
                            "boxxyerrorbars",
                            "[interval]" + re.sub(r'_', r'\\_', data[0].title)
                        ))
                else:
                    plot_cmds.append("  '{0}' using {1} with {2} title columnheader(1)".format(
                        gnuplot_file,
                        "1:3",
                        "steps"
                    ))

            # model files
            for (model_file, style) in self.model_files:
                if not style:
                    style = 'rectangles'
                
                data = DataFile(model_file)
                y_min_data, y_max_data = data.bounds(4)
                y_max = max(y_max_data, y_max)

                gnuplot_file = os.path.join(os.path.dirname(model_file),
                                            "[gnuplot]" + os.path.basename(model_file))
                data.export_for_gnuplot(gnuplot_file, [1,2,3,4])

                size = data.count() if args.show_extra_errors else 1

                plot_cmds.append("  for [IDX=0:{}] '{}' index IDX using {} with {} title columnheader(1)".format(
                    size - 1,
                    gnuplot_file,
                    "1:2:1:2:3:4",
                    "boxxyerrorbars"
                ))
            
            # final plot
            f.write("set xrange [:] noextend\n")
            if y_max > 0:
                f.write("set yrange [0 : {0}]\n".format(y_max * 1.2))

            f.write("plot \\\n")
            f.write(",\\\n".join(plot_cmds))
        return script_file

    def run_gnuplot(self, out_path, image_name):
        image_name = "[gnuplot]" + image_name
        if args.gnuplot == "html":
            image_name += ".html"
        else:
            image_name += ".png"
        script_file = self.create_gnuplot_file(os.path.join(out_path, image_name))
        cmd = [gnuplot, script_file]
        common.run(cmd, log=log)

    def run_racket(self, out_path, image_name):
        image_file = os.path.join(out_path, image_name + ".png")
        cmd = [racket, racket_plot,
               "--out", image_file]
        if self.title:
            cmd += ["--title", self.title]
        if args.width:
            cmd += ["--width", str(args.width)]
        if args.height:
            cmd += ["--height", str(arg.height)]
        if not args.show_extra_errors:
            cmd += ["--single-data"]
        for (error_file, style) in self.error_files:
            if not style:
                style = args.data_plot_style
            cmd += ["--error-data", error_file, args.error, style]
        for (model_file, style) in self.model_files:
            if not style:
                style = 'rectangles'
            cmd += ["--model-data", model_file]
            # cmd += ["--data", model_file, "2,3,4.5", style]

        common.run(cmd, log=log)

    def plot(self):
        # Run plot-data.rkt
        image_name = self.base_name
        if args.type:
            image_name += "-" + args.type
        out_path = output_path
        if args.subexprs:
            out_path = os.path.join(out_path, self.input_name + "-subexprs")
        if not os.path.exists(out_path):
            os.makedirs(out_path)
        if args.gnuplot:
            self.run_gnuplot(out_path, image_name)
        else:
            self.run_racket(out_path, image_name)


class FPTaylorTask:
    def __init__(self, input_files):
        self.cfg_files = []
        if isinstance(input_files, list):
            self.input_files = list(input_files)
        else:
            self.input_files = [input_files]

        self.extra_args = [
            "-v", str(args.verbosity),
            "--opt-approx", "false",
            "--opt-exact", "true",
            "--tmp-base-dir", fptaylor_tmp,
            "--tmp-date", "false",
            "--log-base-dir", fptaylor_log,
            "--log-append-date", "none"
        ]

        if args.type:
            rnd_types = {
                "16": ("float16", "rnd16"),
                "32": ("float32", "rnd32"), 
                "64": ("float64", "rnd64"),
                "real": ("real", "rnd64")
            }
            var_type, rnd_type = rnd_types[args.type]
            self.extra_args += ["--default-var-type", var_type]
            self.extra_args += ["--default-rnd", rnd_type]

        if args.error == 'abs':
            self.extra_args += ["-abs", "true", "-rel", "false", "-ulp", "false"]
        elif args.error == 'rel':
            self.extra_args += ["-abs", "false", "-rel", "true", "-ulp", "false"]
        else:
            self.extra_args += ["-abs", "false", "-rel", "false", "-ulp", "true"]

    def run(self, args):
        cfg_args = []
        for cfg in self.cfg_files:
            cfg_args += ["-c", cfg]
        cmd = [fptaylor] + self.input_files + cfg_args + args + self.extra_args
        common.run(cmd, log=log)


class InputFileTask:
    def __init__(self, input_file):
        self.input_file = input_file
    
    def create_fpcore_file(self, fname):
        out_file = os.path.join(plot_tmp, basename(fname) + ".fpcore")
        cmd = [fptaylor, fname,
               "--fpcore-out", out_file,
               "--tmp-base-dir", fptaylor_tmp,
               "--log-base-dir", fptaylor_log,
               "--log-append-date", "none",
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
        return out_file, fpcore_type

    def run(self):
        if not os.path.isfile(self.input_file):
            log.error("Input file does not exist: {0}".format(self.input_file))
            sys.exit(1)
        
        out_path = os.path.join(plot_tmp, os.path.basename(self.input_file))
        shutil.copy(self.input_file, out_path)
        
        if args.range:
            restrict_input_vars(out_path, args.range)

        if args.subexprs:
            fpcore_file, fpcore_type = self.create_fpcore_file(out_path)
            cmd = [racket, core2fptaylor,
                   "--var-precision", fpcore_type,
                   "--subexprs"]
            with open(out_path, 'w') as f:
                common.run(cmd + ["--", fpcore_file], log=log, stdout=f)

        return out_path


class GappaTask:
    def __init__(self, input_file):
        self.input_file = input_file

    def run(self):
        gappa_data = os.path.join(base_path, "gappa_data.py")
        if args.error != "ulp":
            error = args.error
        else:
            log.warning("Gappa does not support the ULP error")
            error = "rel"
        cmd = [gappa_data,
               "--error", error,
               "--type", args.type,
               "--segments", str(args.gappa_segments),
               "--output-path", plot_tmp,
               "--", self.input_file]
        common.run(cmd, log=log)


for input_file in args.input:
    fname = InputFileTask(input_file).run()
    base_fname = basename(fname) + "-" + args.error
    if args.range:
        base_fname += "-range"
    if args.mpfr_prec:
        base_fname += "-mpfr{0}".format(args.mpfr_prec)
    if args.mpfi:
        base_fname += "-mpfi"

    error_bounds_file_template = None
    common.remove_all(plot_tmp, base_fname + "*")
    
    plot_tasks = dict()

    for cfg_files in args.config:
        fptaylor_task = FPTaylorTask(fname)

        if not cfg_files:
            # default config
            cfg_name = "default"
        else:
            cfg_name = "-".join([basename(cfg) for cfg in cfg_files])
            for cfg_file in cfg_files:
                if not os.path.isfile(cfg_file):
                    log.error(
                        "Configuration file does not exist: {0}".format(cfg_file))
                    sys.exit(1)
                fptaylor_task.cfg_files.append(cfg_file)

        export_args = []

        if not error_bounds_file_template:
            error_bounds_file_template = os.path.join(plot_tmp, base_fname + "-{task}.c")
            export_args += ["--export-error-bounds", error_bounds_file_template]

        c_model_file_template = os.path.join(
            plot_tmp, "model-" + base_fname + "-" + cfg_name + "-{task}.c")
        export_args += ["--export-error-bounds-data", c_model_file_template]

        fptaylor_task.run(export_args)

        for task, model_file in files_from_template(c_model_file_template).items():
            # Adjust names in the output model file
            common.replace_in_file(model_file,
                                   [(r"f_names\[\] =", '"([^"]*)"', r'"\1-{0}"'.format(cfg_name))])
            data_file = run_data_mpfi(model_file)
            if task not in plot_tasks:
                plot_tasks[task] = PlotTask(base_fname, "[{0}]{1}".format(task, base_fname))
            plot_task = plot_tasks[task]
            plot_task.add_model_file(data_file)
            if args.subexprs:
                title = common.find_in_file(model_file, 
                                            r'expression_string = "([^"]*)";',
                                            groups=1)
                if title:
                    plot_task.title = title

    # ErrorBounds
    for task, input_file in files_from_template(error_bounds_file_template).items(): 
        data_file = run_error_bounds(input_file)
        if task not in plot_tasks:
            log.warning("Undefined task '{0}' for the data file '{1}'".format(task, input_file))
            plot_tasks[task] = PlotTask(base_fname, "[{0}]{1}".format(task, base_fname))
        plot_tasks[task].add_error_file(data_file)

    # Gappa
    if args.gappa:
        gappa = GappaTask(fname)
        gappa.run()
        results = files_from_template(os.path.join(plot_tmp, "gappa-data-{task}.txt"))
        for task, data_file in results.items():
            if task not in plot_tasks:
                log.warning("Undefined task '{0}' for the data file '{1}'".format(task, data_file))
                plot_tasks[task] = PlotTask(base_fname, "[{0}]{1}".format(task, base_fname))
            plot_tasks[task].add_error_file(data_file, style="lines")

    # plot-fptaylor.rkt
    for task in plot_tasks.values():
        task.plot()

