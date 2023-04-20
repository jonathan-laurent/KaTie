# The Kappa Trace Inquiry Engine

This tool implements a unified language for querying simulation traces of rule-based models written in Kappa about the statistical behavior of individual agents and the relative frequency of different causal patterns. This language was first described in this [paper](https://www.cs.cmu.edu/~jlaurent/pdf/papers/cmsb18.pdf).


## Installation Instructions

The simplest way to install KaTie is via opam:

- Install [`opam`](https://opam.ocaml.org/doc/Install.html).
- Install the [`Kappa Tools`](https://github.com/Kappa-Dev/KappaTools): `opam pin add -y https://github.com/Kappa-Dev/KappaTools.git`.
- Install KaTie: `opam pin add -y kappa-trace-queries https://github.com/jonathan-laurent/KaTie.git`
- The tool can then be used as `KaTie -t <trace_file> -q <query_file> [options]`.

## Testing Instructions

The `tests` directory contains a collection of example models and queries. A subdirectory is considered as defining a valid test if it features a `model.ka` and a `query.katie` file. The `exec.sh` script can be used to run KaSim and KaTie in sequence on a test.

There are two ways to specify the expected outcome of running a test:

1. An `expected/results` folder can be placed in the test directory. The testing script checks that the content of `katie-output` matches the content of `expected/results`.
2. The expected behavior of a query can be encoded in the name of the query itself. If a query's name contains the substring `__matches_N` with `N` a nonnegative integer, then it is expected to produce `N` matches exactly. In addition, if it contains the substring `__all_true`, then every element of the resulting CSV must be equal to `1`. Finally, if it contains the substring `errors__`, then the query must be detected as invalid **statically** (before execution starts).

### Using the testing script

The testing script `runtests.py` can be used as follows:

```
python runtests.py [TEST_SUBDIRS] {run,clean,diff,promote}.
```

Here, `TEST_SUBDIRS` is a list of sub-directories indicating which tests must be targeted by the command that follows. If no tests are provided, all tests in the `tests` hierarchy are implicitely included. The available commands are as follows:

- `run`: run a test. KaTie's output is stored in `katie-output`. The `stdout` and `stderr` channels are captured and their content stored into `katie-output` too. Test failures are shown in red.
- `clean`: clean up a test's directory of all non-versioned auto-generated files.
- `diff`: whenever `run` indicates an unexpected output, this command can be used to show a `diff`.
- `promote`: update the `expected` folder with the current content of `katie-output`.

Here are some example commands:

```sh
python runtests.py run  # run the full test suite
python runtests.py clean  # clean-up everything
python runtests.py tests/large/catphos run  # run the 'catphos' test
python runtests.py tests/large/catphos diff  # show a diff for 'catphos'
```

### Contributing new tests

Users are encouraged to contribute new tests via pull-requests. To add a new test, just add a sub-directory in the `tests` hierarchy and make sure that it works with `runtests.py`.

Here are some tips for debugging and inspectign tests:

- The `runtests.py` script captures the standard output and uses specific KaTie options that must not be changed. To explore running the test with other options or just have the standard output printed on your terminal, you can use the `exec.sh` script instead.
- The `runtests.py` script runs KaTie with a maximal debug level, meaning that a lot of useful files are generated in the `katie-output/debug` directory to help understanding how the trace was processed. In particular, `trace-summary.json` contains a summary of the trace that is friendlier than the original `trace.json` file and `matchings.json` enumerates all found matchings.

In addition to ensuring valid queries are correctly executed, it is also important to ensure that as many invalid queries as possible are detected as such statically (i.e. before they are executed) to avoid having queries failing at runtime, possibly wasting weeks of computation. Thus, users are encouraged to add invalid queries to the tests while including the `errors__` substring in their name.