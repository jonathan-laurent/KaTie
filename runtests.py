"""
A script for running the TQL test suite.

Usage: python runtests.py {run,clean} [test_subdir].
"""

import os
from os.path import join
import shutil
import sys

KATIE_EXE = "dune exec KaTie --"
MODEL_FILE = "model.ka"
TRACE_FILE = "trace.json"
QUERY_FILE = "query.katie"
KASIM_OUT_DIR = "kasim-output"
KATIE_OUT_DIR = "katie-output"
TESTS_DIR = "tests"


def run(dir):
    model = join(dir, MODEL_FILE)
    kasim_out = join(dir, KASIM_OUT_DIR)
    katie_out = join(dir, KATIE_OUT_DIR)
    query = join(dir, QUERY_FILE)
    os.system(f"KaSim {model} -seed 0 -trace {TRACE_FILE} -d {kasim_out}")
    tql_options = f"--output-directory {katie_out} --debug --no-color"
    os.system(f"{KATIE_EXE} -t {join(kasim_out, TRACE_FILE)} -q {query} {tql_options}")


def clean(dir):
    for sub in [KASIM_OUT_DIR, KATIE_OUT_DIR]:
        shutil.rmtree(join(dir, sub), ignore_errors=True)


def valid_test_dir(dir):
    return all(os.path.isfile(join(dir, f)) for f in [MODEL_FILE, QUERY_FILE])


def find_all_test_dirs():
    for root, subdirs, files in os.walk(TESTS_DIR):
        for s in subdirs:
            dir = join(root, s)
            if valid_test_dir(dir):
                yield dir


def test_dirs(args):
    if not args:
        for dir in find_all_test_dirs():
            yield dir
    else:
        for a in args:
            dir = join(TESTS_DIR, a)
            assert valid_test_dir(dir), f"Invalid test directory: {dir}"
            yield dir


if __name__ == "__main__":
    if len(sys.argv) <= 1:
        pass  # run all tests
    else:
        cmd = sys.argv[1]
        args = sys.argv[2:]
        if cmd == "clean":
            for dir in test_dirs(args):
                clean(dir)
        elif cmd == "run":
            for dir in test_dirs(args):
                run(dir)
        elif cmd == "list":
            for dir in find_all_test_dirs():
                print(dir)
