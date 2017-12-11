import sys
import os
import re
import subprocess
import shutil
import glob
import logging
import hashlib


def get_log():
    log = logging.getLogger()
    log.setLevel(logging.INFO)
    handler = logging.StreamHandler()
    handler.setFormatter(logging.Formatter("[%(levelname)s] %(message)s"))
    log.addHandler(handler)
    return log


def run(cmd, ignore_return_codes=[], stdout=None, log=None, silent=False):
    if not silent:
        msg = "Running: {0}".format(" ".join(cmd))
        if log:
            log.info(msg)
        else:
            print(msg)
    ret = subprocess.call(cmd, stdout=stdout)
    if ret != 0 and ret not in ignore_return_codes:
        msg = "Return code: {0}".format(ret)
        if log:
            log.error(msg)
        else:
            print(msg)
        sys.exit(2)
    return ret


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


def find_in_cache(cache_path, input_file, args):
    h = get_hash(input_file, args)
    if not h:
        return None
    fname = os.path.join(cache_path, h)
    if os.path.isfile(fname):
        return fname
    return None


def cache_file(cache_path, fname, input_file, args):
    if not os.path.isfile(fname):
        return
    h = get_hash(input_file, args)
    if not h:
        return
    out = os.path.join(cache_path, h)
    shutil.copy(fname, out)


def replace_in_file(fname, pats, out_name=None, log=None):
    """Replaces patterns in a given file."""
    if not os.path.isfile(fname):
        if log:
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


def find_in_file(fname, pat, groups=None):
    if not os.path.isfile(fname):
        return
    with open(fname, 'r') as f:
        text = f.read()
    m = re.search(pat, text)
    if not m:
        return None
    if not groups:
        return m.group(0)
    if not isinstance(groups, list):
        return m.group(groups)
    result = []
    for group in groups:
        result.append(m.group(group))
    return result


def remove_all(base, name_pat):
    for f in glob.glob(os.path.join(base, name_pat)):
        if os.path.isfile(f):
            os.remove(f)


def remove_files(files):
    for f in files:
        if os.path.isfile(f):
            os.remove(f)
