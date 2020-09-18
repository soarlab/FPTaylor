import subprocess
import os
import sys
import glob
import logging
import argparse
import re
import yaml

fptaylor = '../fptaylor'

def get_log():
    log = logging.getLogger()
    log.setLevel(logging.INFO)
    handler = logging.StreamHandler()
    handler.setFormatter(logging.Formatter("[%(levelname)s] %(message)s"))
    log.addHandler(handler)
    return log

log = get_log()

def run_output(cmd, ignore_return_codes=[], log=None, silent=False):
    if not silent:
        msg = "Running: {0}".format(" ".join(cmd))
        if log:
            log.info(msg)
        else:
            print(msg)
    try:
        return subprocess.check_output(cmd, stderr=subprocess.STDOUT)
    except subprocess.CalledProcessError as e:
        if e.returncode in ignore_return_codes:
            return e.output
        msg = "{0}\n\nReturn code: {1}".format(e.output, e.returncode)
        if log:
            log.error(msg)
        else:
            print(msg)
        sys.exit(2)

class FPTaylorExpression:
    patterns = [
        ('abs-error', r'Absolute error [^:]*: ([-+\de.]+)'),
        ('abs-error-hex', r'Absolute error [^:]*: [^\(]* \(([-+\da-fxp.]+)\)'),
        ('time', r'Elapsed time: ([\d.]+)')
    ]

    def __init__(self, data):
        self.name = data['name']
        self.upper_bound = data.get('upper-bound', None)
        self.lower_bound = data.get('lower-bound', None)
        self.exact_value = data.get('exact-value', None)
        self.time = data.get('time', None)

    def to_dict(self):
        res = {'name': self.name}
        if self.upper_bound is not None:
            res['upper-bound'] = self.upper_bound
        if self.lower_bound is not None:
            res['lower-bound'] = self.lower_bound
        if self.exact_value is not None:
            res['exact-value'] = self.exact_value
        if self.time is not None:
            res['time'] = self.time
        return res

    def select_output_lines(self, output):
        res = []
        lines = output.split('\n')
        index = lines.index('Problem: ' + self.name)
        for line in lines[index + 1:]:
            if re.match(r'Problem: .+', line):
                break
            res.append(line)
        return res

    def parse_output(self, lines):
        res = {}
        for line in lines:
            for v, pat in self.patterns:
                m = re.match(pat, line, re.I)
                if not m: continue
                res[v] = m.group(1)
        return res

    def generate(self, output):
        vals = self.parse_output(self.select_output_lines(output))
        if 'abs-error' in vals:
            self.upper_bound = float(vals['abs-error'])
        if 'abs-error-hex' in vals:
            self.exact_value = vals['abs-error-hex']
        if 'time' in vals:
            self.time = float(vals['time'])

    def check(self, output):
        vals = self.parse_output(self.select_output_lines(output))
        if self.upper_bound is not None:
            v = float(vals['abs-error'])
            if v > self.upper_bound:
                log.error(f'Incorrect upper bound: actual = {v} > expected = {self.upper_bound}')
        if self.lower_bound is not None:
            v = float(vals['abs-error'])
            if v < self.lower_bound:
                log.error(f'Incorrect lower bound: actual = {v} < expected = {self.lower_bound}')
        if self.exact_value is not None:
            v = vals['abs-error-hex']
            if v != self.exact_value:
                log.error(f'Incorrect exact value: actual = {v} != expected = {self.exact_value}')



class FPTaylorFile:
    def __init__(self, data):
        if isinstance(data, str):
            self.name = data
            self.expressions = []
        else:
            self.name = data['name']
            self.expressions = [FPTaylorExpression(d) for d in data['expressions']]

    def to_dict(self):
        res = {
            'name': self.name,
            'expressions': [expr.to_dict() for expr in self.expressions]
        }
        return res

    def run(self, args=[]):
        extra_args = [
            '-v', '0',
            '--print-hex-floats', 'true',
            # "--opt-approx", "false",
            # "--opt-exact", "true",
            # "--tmp-base-dir", fptaylor_tmp,
            # "--tmp-date", "false",
            # "--log-base-dir", fptaylor_log,
            "--log-append-date", "none"
        ]

        # if args.type:
        #     rnd_types = {
        #         "16": ("float16", "rnd16"),
        #         "32": ("float32", "rnd32"), 
        #         "64": ("float64", "rnd64"),
        #         "real": ("real", "rnd64")
        #     }
        #     var_type, rnd_type = rnd_types[args.type]
        #     self.extra_args += ["--default-var-type", var_type]
        #     self.extra_args += ["--default-rnd", rnd_type]

        # if args.error == 'abs':
        #     self.extra_args += ["-abs", "true", "-rel", "false", "-ulp", "false"]
        # elif args.error == 'rel':
        #     self.extra_args += ["-abs", "false", "-rel", "true", "-ulp", "false"]
        # else:
        #     self.extra_args += ["-abs", "false", "-rel", "false", "-ulp", "true"]

        cmd = [fptaylor] + [self.name] + args + extra_args
        output = run_output(cmd).decode()
        return output

    def generate_tests(self, args=[]):
        self.expressions.clear()
        output = self.run(args)
        for line in output.split('\n'):
            m = re.match(r'Problem: (.+)', line)
            if m:
                expr = FPTaylorExpression({'name': m.group(1)})
                expr.generate(output)
                self.expressions.append(expr)


    def run_tests(self, args=[]):
        output = self.run(args)
        for expr in self.expressions:
            expr.check(output)

    # def run(self, args):
    #     cfg_args = []
    #     for cfg in self.cfg_files:
    #         cfg_args += ["-c", cfg]
    #     cmd = [fptaylor] + self.input_files + cfg_args + args + self.extra_args
    #     common.run(cmd, log=log)


# data = [{'file': 'jet.txt', 'tasks': {'name': 'jet', 'upper-bound': 1.11e-11, 'lower-bound': 1.0e-10}}]

# with open('aaa.yml', 'w') as f:
#     yaml.dump(data, f, sort_keys=False)

def parse_args():
    parser = argparse.ArgumentParser(
        description='Test runner for FPTaylor')
    parser.add_argument('--generate', 
        help='generate tests from files in the given directory')

    args = parser.parse_args()
    return args

def main():
    args = parse_args()

    if args.generate:
        files = []
        for fname in glob.glob(os.path.join(args.generate, '*.txt')):
            f = FPTaylorFile(fname)
            f.generate_tests()
            files.append(f.to_dict())
        with open('aaa.yml', 'w') as f:
            yaml.dump({'files': files}, f, sort_keys=False)
    else:
        with open('aaa.yml', 'r') as f:
            tests = yaml.safe_load(f)

        files = [FPTaylorFile(f) for f in tests['files']]
        for f in files:
            f.run_tests()

if __name__ == '__main__':
    main()