"""
A script for running the TQL test suite.

Usage: python runtests.py [TEST_SUBDIRS] {run,clean,diff,promote}.
"""

import csv
import os
import re
import shutil
import sys
import subprocess
from os.path import join
import argparse

KATIE_EXE = ["dune", "exec", "KaTie", "--"]
KASIM_EXE = ["KaSim"]
MODEL_FILE = "model.ka"
TRACE_FILE = "trace.json"
QUERY_FILE = "query.katie"
KASIM_OUT_DIR = "kasim-output"
KATIE_OUT_DIR = "katie-output"
RESULTS_DIR = "results"
STDOUT_FILE = "stdout.txt"
STDERR_FILE = "stderr.txt"
TESTS_DIR = "tests"
EXPECTED_DIR = "expected"

VERBOSE = False  # overriden by the --verbose option


# We do not want to introduce Python dependencies
def red(s):
    return "\x1b[1;31m" + s + "\x1b[0m"


def green(s):
    return "\x1b[1;32m" + s + "\x1b[0m"


def expected_files(dir):
    # `dir` is supposed to be the KaTie output diectory.
    for f in os.listdir(join(dir, RESULTS_DIR)):
        if os.path.splitext(f)[-1] == ".csv":
            yield join(RESULTS_DIR, f)
    if os.path.isfile(join(dir, STDOUT_FILE)):
        yield STDOUT_FILE


def correct_csv(file, *, num_matches=None, all_ones=False):
    with open(file, "r") as f:
        lines = list(csv.reader(f))
        if num_matches is not None:
            if len(lines) != num_matches + 1:  # // KaTie adds a blank line at the end
                return False
        if all_ones:
            if not all(all(x.strip() == "1" for x in l) for l in lines):
                return False
    return True


def autocheck_result(dir):
    out = join(dir, KATIE_OUT_DIR)
    for f in expected_files(out):
        if m := re.search(r"__matches_(\d+)", f):
            num_matches = int(m.group(1))
        else:
            num_matches = None
        all_ones = bool(re.search(r"__all_true", f))
        if num_matches is not None or all_ones:
            ok = correct_csv(join(out, f), num_matches=num_matches, all_ones=all_ones)
            if ok and VERBOSE:
                print(green(f"Autocheck '{f}': OK."))
            if not ok:
                print(red(f"Autocheck '{f}': FAIL."))


def update_expected(dir):
    expected = join(dir, EXPECTED_DIR)
    if not os.path.isdir(expected):
        return
    for f in expected_files(expected):
        print(f"Updating {join(expected, f)}...")
        shutil.copyfile(join(dir, KATIE_OUT_DIR, f), join(expected, f))


def check_diff(dir, verbose=True):
    expected = join(dir, EXPECTED_DIR)
    if not os.path.isdir(expected):
        return
    for f in expected_files(expected):
        expected_file = join(expected, f)
        actual_file = join(dir, KATIE_OUT_DIR, f)
        if not os.path.isfile(actual_file):
            print(red(f"Missing expected file: {actual_file}"))
        with open(expected_file, "r") as ef, open(actual_file, "r") as af:
            if ef.read() != af.read():
                if verbose:
                    # Use the OS diff program to show the diff
                    subprocess.run(
                        ["diff", "--color", "-u", expected_file, actual_file]
                    )
                    print()
                else:
                    print(red(f"Unexpected output: {f}"))


def run_cmd(cmd):
    result = subprocess.run(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
    )
    return result.returncode, result.stdout, result.stderr


def run_cmd_saving_output(cmd, stdout_file, stderr_file):
    status, stdout, stderr = run_cmd(cmd)
    os.makedirs(os.path.dirname(stdout_file), exist_ok=True)
    os.makedirs(os.path.dirname(stderr_file), exist_ok=True)
    with open(stdout_file, "w") as f:
        f.write(stdout)
    with open(stderr_file, "w") as f:
        f.write(stderr)
    if VERBOSE:
        print(stdout)
        print(stderr, file=sys.stderr)
    return status


def run(dir):
    clean(dir)
    print(f"Running test in: {dir}...")
    model = join(dir, MODEL_FILE)
    kasim_out = join(dir, KASIM_OUT_DIR)
    katie_out = join(dir, KATIE_OUT_DIR)
    query = join(dir, QUERY_FILE)
    kasim_ret = run_cmd_saving_output(
        KASIM_EXE + [model, "-seed", "0", "-trace", TRACE_FILE, "-d", kasim_out],
        stdout_file=join(kasim_out, STDOUT_FILE),
        stderr_file=join(kasim_out, STDERR_FILE),
    )
    if kasim_ret != 0:
        print(f"KaSim failed: see {join(kasim_out, STDERR_FILE)}")
        return
    katie_args = ["-t", join(kasim_out, TRACE_FILE), "-q", query]
    katie_options = [
        "--debug-level",
        "2",
        "--output-dir",
        katie_out,
        "--no-color",
    ]
    katie_ret = run_cmd_saving_output(
        KATIE_EXE + katie_args + katie_options,
        stdout_file=join(katie_out, STDOUT_FILE),
        stderr_file=join(katie_out, STDERR_FILE),
    )
    if katie_ret != 0:
        print(f"KaTie return code: {katie_ret}")
        print(f"  See {join(katie_out, STDOUT_FILE)}")
        print(f"  See {join(katie_out, STDERR_FILE)}")


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
        for dir in args:
            assert valid_test_dir(dir), f"Invalid test directory: {dir}"
            yield dir


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="runtests.py", description="Test launcher for KaTie."
    )
    parser.add_argument(
        "tests",
        nargs="*",
        metavar="TEST",
        help="List of tests to run the command on (everything by default)",
    )
    parser.add_argument(
        "command",
        choices=["run", "clean", "diff", "promote"],
        help="Command to execute",
    )
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="Increase output verbosity"
    )
    args = parser.parse_args()
    VERBOSE = args.verbose
    if args.command == "clean":
        for dir in test_dirs(args.tests):
            clean(dir)
    elif args.command == "run":
        for dir in test_dirs(args.tests):
            run(dir)
            autocheck_result(dir)
            check_diff(dir, verbose=False)
    elif args.command == "list":
        for dir in find_all_test_dirs():
            print(dir)
    elif args.command == "diff":
        for dir in test_dirs(args.tests):
            check_diff(dir)
    elif args.command == "promote":
        for dir in test_dirs(args.tests):
            update_expected(dir)
