import re
import os
from decimal import Decimal
from math import isclose
import logging
import common
import config

_log = logging.getLogger()

class Config:
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


class FPTaylorExpression:
    patterns = [
        ('abs-error', r'Absolute error [^:]*: ([-+\de.]+)'),
        ('abs-error-hex', r'Absolute error [^:]*: [^\(]* \(([-+\da-fxp.]+)\)'),
        ('abs-total2-hex', r'Second order absolute error [^:]*: [^\(]* \(([-+\da-fxp.]+)\)'),
        ('time', r'Elapsed time: ([\d.]+)')
    ]

    def __init__(self, data):
        self.name = data['name']
        self.upper_bound = data.get('upper-bound', None)
        self.lower_bound = data.get('lower-bound', None)
        self.exact_value = data.get('exact-value', None)
        self.total2 = data.get('total2', None)
        self.time = data.get('time', None)

    def to_dict(self):
        res = {'name': self.name}
        if self.upper_bound is not None:
            res['upper-bound'] = self.upper_bound
        if self.lower_bound is not None:
            res['lower-bound'] = self.lower_bound
        if self.exact_value is not None:
            res['exact-value'] = self.exact_value
        if self.total2 is not None:
            res['total2'] = self.total2
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
            self.upper_bound = vals['abs-error']
        if 'abs-error-hex' in vals:
            self.exact_value = vals['abs-error-hex']
        if 'abs-total2-hex' in vals:
            self.total2 = vals['abs-total2-hex']
        if 'time' in vals:
            self.time = float(vals['time'])

    @staticmethod
    def check_hex(title, actual, expected, rel_tol=0.0):
        if rel_tol > 0:
            actual = common.hex_to_float(actual)
            expected = common.hex_to_float(expected)
            if not isclose(actual, expected, rel_tol=rel_tol):
                _log.error(f'Incorrect {title} (with rel-tol={rel_tol}): actual = {actual} != expected = {expected}')
                return False
        elif actual != expected:
            _log.error(f'Incorrect {title}: actual = {actual} != expected = {expected}')
            return False
        return True

    def check(self, output, rel_tol=0.0):
        print(f'  {self.name}: ', end='', flush=True)
        vals = self.parse_output(self.select_output_lines(output))
        passed = True
        if self.upper_bound is not None:
            v = Decimal(vals['abs-error'])
            if v > Decimal(self.upper_bound):
                _log.error(f'Incorrect upper bound: actual = {v} > expected = {self.upper_bound}')
                passed = False
        if self.lower_bound is not None:
            v = Decimal(vals['abs-error'])
            if v < Decimal(self.lower_bound):
                _log.error(f'Incorrect lower bound: actual = {v} < expected = {self.lower_bound}')
                passed = False
        if self.exact_value is not None:
            passed &= self.check_hex('exact value', vals['abs-error-hex'], self.exact_value, rel_tol)
        if self.total2 is not None:
            passed &= self.check_hex('total2', vals['abs-total2-hex'], self.total2, rel_tol)
        print('PASSED' if passed else 'FAILED')
        return passed


class FPTaylorFile:
    def __init__(self, data, base_path=''):
        if isinstance(data, str):
            self.name = data
            self.expressions = []
        else:
            self.name = data['name']
            self.expressions = [FPTaylorExpression(d) for d in data['expressions']]
        self.base_path = base_path
        self.name = os.path.join(base_path, self.name)

    def to_dict(self):
        res = {
            'name': os.path.relpath(self.name, self.base_path) if self.base_path else self.name,
            'expressions': [expr.to_dict() for expr in self.expressions]
        }
        return res

    def run(self, args=[], silent=False):
        extra_args = [
            '-v', '0',
            # '--print-hex-floats', 'true',
            # "--tmp-base-dir", fptaylor_tmp,
            # "--tmp-date", "false",
            # "--log-base-dir", fptaylor_log,
            "--log-append-date", "none"
        ]
        cmd = [config.fptaylor_exe] + args + extra_args + [self.name]
        output = common.run_output(cmd, silent=silent).decode()
        return output

    def generate_tests(self, args=[], export_options=None):
        self.expressions.clear()
        extra_args = []
        if export_options:
            extra_args.append('--export-options')
            extra_args.append(export_options)
        output = self.run(args + extra_args)
        for line in output.split('\n'):
            m = re.match(r'Problem: (.+)', line)
            if m:
                expr = FPTaylorExpression({'name': m.group(1)})
                expr.generate(output)
                self.expressions.append(expr)

    def run_tests(self, args=[], rel_tol=0.0):
        print(f'Testing: {self.name}', flush=True)
        if rel_tol > 0:
            print(f'rel-tol = {rel_tol}')
        output = self.run(args, silent=True)
        passed, failed = 0, 0
        for expr in self.expressions:
            if expr.check(output, rel_tol=rel_tol):
                passed += 1
            else:
                failed += 1
        return passed, failed
