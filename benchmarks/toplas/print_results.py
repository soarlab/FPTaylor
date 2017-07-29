#!/usr/bin/env python

import sys
import os
import glob
import decimal

benchmark_list = [
    "t_div_t1",
    "sine",
    "sqroot",
    "sineOrder3",
    "carbon_gas",
    "verhulst",
    "predatorPrey",
    "rigidBody1",
    "rigidBody2",
    "doppler1",
    "doppler2",
    "doppler3",
    "turbine1",
    "turbine2",
    "turbine3",
    "jet",
    "logexp",
    "sphere",
    "azimuth",
    "kepler0",
    "kepler1",
    "kepler2",
    "himmilbeau",
    "hartman3",
    "hartman6",
    "floudas1",
    "floudas2",
    "floudas3"
]

class Problem:
    def __init__(self, name, error, time):
        self.name = name
        self.error_str = "{0:.2e}".format(
            decimal.Context(prec=3, rounding=decimal.ROUND_UP).create_decimal(error))
        self.time_str = "{0:.1f}".format(time)

    def __str__(self):
        return "{0}, {1}, {2}".format(self.name, self.error_str, self.time_str)

def problem_from_file(fname):
    name = None
    err_abs = None
    err_rel = None
    time = None
    with open(fname, 'r') as f:
        for line in f:
            if line.startswith("Problem: "):
                name = line[len("Problem: "):].strip()
            elif line.startswith("Absolute error (exact): "):
                err_abs = line[len("Absolute error (exact): "):].strip()
            elif line.startswith("Absolute error (approximate): "):
                err_abs = line[len("Absolute error (approximate): "):].strip()
            elif line.startswith("Relative error (exact): "):
                err_rel = line[len("Relative error (exact): "):].strip()
            elif line.startswith("Relative error (approximate): "):
                err_rel = line[len("Relative error (approximate): "):].strip()
            elif line.startswith("Elapsed time: "):
                time = float(line[len("Elapsed time: "):].strip())
    if name and (err_abs or err_rel) and time:
        return Problem(name, err_abs if err_abs else err_rel, time)
    else:
        return None

base_dir = "log"
if len(sys.argv) == 2:
    base_dir = sys.argv[1]

results = {}
    
for fname in glob.glob(os.path.join(base_dir, "*.log")):
    result = problem_from_file(fname)
    if result:
        results[result.name] = result

for name in benchmark_list:
    if name in results:
        print(results[name])
        del results[name]

if len(results) > 0:
    print("\nUnsorted results:")    
    for _, result in results.iteritems():
        print(result)
    
