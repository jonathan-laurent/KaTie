"""
A script for running the TQL test suite.

Usage: python runtests.py {run,clean,diff,promote} [test_subdir].
"""

import os
import shutil
import sys
import subprocess
from os.path import join

KATIE_EXE = ["dune", "exec", "KaTie", "--"]
KASIM_EXE = ["KaSim"]
MODEL_FILE = "model.ka"
TRACE_FILE = "trace.json"
QUERY_FILE = "query.katie"
KASIM_OUT_DIR = "kasim-output"
KATIE_OUT_DIR = "katie-output"
STDOUT_FILE = "stdout.txt"
STDERR_FILE = "stderr.txt"
TESTS_DIR = "tests"
EXPECTED_DIR = "expected"


def expected_files(dir):
    for f in os.listdir(dir):
        if os.path.splitext(f)[-1] == ".csv":
            yield f
        elif f == STDOUT_FILE:
            yield f


def update_expected(dir):
    expected = join(dir, EXPECTED_DIR)
    if not os.path.isdir(expected):
        return
    for f in expected_files(expected):
        print(f"Updating {join(expected, f)}...")
        shutil.copyfile(join(dir, KATIE_OUT_DIR, f), join(expected, f))


def check_diff(dir):
    expected = join(dir, EXPECTED_DIR)
    if not os.path.isdir(expected):
        return
    for f in expected_files(expected):
        expected_file = join(expected, f)
        actual_file = join(dir, KATIE_OUT_DIR, f)
        if not os.path.isfile(actual_file):
            print(f"Missing file: {actual_file}")
        with open(expected_file, "r") as ef, open(actual_file, "r") as af:
            if ef.read() != af.read():
                # Use the OS diff program to show the diff
                subprocess.run(["diff", "--color", "-u", expected_file, actual_file])
                print()


def run_cmd(cmd):
    result = subprocess.run(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
    )
    return result.returncode, result.stdout, result.stderr


def run_cmd_saving_output(cmd, stdout_file, stderr_file):
    status, stdout, stderr = run_cmd(cmd)
    with open(stdout_file, "w") as f:
        f.write(stdout)
    with open(stderr_file, "w") as f:
        f.write(stderr)
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
    katie_options = ["--output-directory", katie_out, "--debug", "--no-color"]
    katie_ret = run_cmd_saving_output(
        KATIE_EXE + katie_args + katie_options,
        stdout_file=join(katie_out, STDOUT_FILE),
        stderr_file=join(katie_out, STDERR_FILE),
    )
    print(f"KaTie return code: {katie_ret}")


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
    cmd = sys.argv[1] if len(sys.argv) >= 1 else "run"
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
    elif cmd == "diff":
        for dir in test_dirs(args):
            check_diff(dir)
    elif cmd == "promote":
        for dir in test_dirs(args):
            update_expected(dir)
    else:
        print("Invalid command: " + cmd)
        exit(1)
