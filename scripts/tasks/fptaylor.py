import re
import os
from decimal import Decimal
import logging
import common
from config import fptaylor_exe

_log = logging.getLogger()


class FPTaylorConfig:
    def __init__(self, lst, base_path=''):
        self.args = []
        for arg in lst:
            self.add(arg, base_path=base_path)

    def add(self, arg, base_path=''):
        if '=' in arg:
            lhs, rhs = arg.split('=')
            self.args.append('--' + lhs.strip())
            self.args.append(rhs.lstrip())
        else:
            self.args.append('-c')
            self.args.append(os.path.join(base_path, arg))


class FPTaylorResults:
    patterns = [
        ('name', r'Problem: ([\w_]+)'),
        ('abs-error', r'Absolute error [^:]*: ([-+\de.]+)'),
        ('abs-error-hex', r'Absolute error [^:]*: [^\(]* \(([-+\da-fxp.]+)\)'),
        ('time', r'Elapsed time: ([\d.]+)')
    ]

    def __init__(self, output):
        self.results = {}
        lines = output.split('\n')
        guard = '-' * 79
        try:
            start = lines.index(guard)
        except:
            start = -1
        while start >= 0:
            try:
                end = lines.index(guard, start + 1)
            except: 
                end = -1
            res = self.parse_result(lines[start:end])
            self.results[res['name']] = res
            start = end

    def __getitem__(self, name):
        return self.results[name]

    def parse_result(self, lines):
        res = {}
        for line in lines:
            for v, pat in FPTaylorResults.patterns:
                m = re.match(pat, line, re.I)
                if not m: continue
                res[v] = m.group(1)
        return res


class FPTaylorTask:
    def __init__(self, files):
        if isinstance(files, list):
            self.input_files = list(files)
        else:
            self.input_files = [files]

    def run(self, config, silent=False):
        extra_args = [
            '-v', '0',
            # '--print-hex-floats', 'true',
            # "--tmp-base-dir", fptaylor_tmp,
            # "--tmp-date", "false",
            # "--log-base-dir", fptaylor_log,
            "--log-append-date", "none"
        ]
        cmd = [config.fptaylor_exe] + extra_args + config.args + self.files
        output = common.run_output(cmd, silent=silent).decode()
        return FPTaylorResults(output)