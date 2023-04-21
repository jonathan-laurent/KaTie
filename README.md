# The Kappa Trace Inquiry Engine

This tool implements a unified language for querying simulation traces of rule-based models written in Kappa about the statistical behavior of individual agents and the relative frequency of different causal patterns. This language was first described in this [paper](https://www.cs.cmu.edu/~jlaurent/pdf/papers/cmsb18.pdf).


* [Installation Instructions](#installation-instructions)
* [Tutorial](#tutorial)
* [Reference](#reference)
* [Implementation Details](#implementation-details)
* [Frequently Asked Questions](#frequently-asked-questions)
* [Testing Instructions](#testing-instructions)
  + [Using the testing script](#using-the-testing-script)
  + [Contributing new tests](#contributing-new-tests)


## Installation Instructions

The simplest way to install KaTie is via opam:

- Install [`opam`](https://opam.ocaml.org/doc/Install.html).
- Install the [`Kappa Tools`](https://github.com/Kappa-Dev/KappaTools): `opam pin add -y https://github.com/Kappa-Dev/KappaTools.git`.
- Install KaTie: `opam pin add -y kappa-trace-queries https://github.com/jonathan-laurent/KaTie.git`
- The tool can then be used as `KaTie -t <trace_file> -q <query_file> [options]`.

## Tutorial

We introduce the _trace query language_ and its execution engine KaTie using a simple example involving a substrate-kinase system (`tests/large/catphos`). The example is discussed in more details in the [paper](https://www.cs.cmu.edu/~jlaurent/pdf/papers/cmsb18.pdf).

```
%agent: K(d, x{u,p})
%agent: S(d, x{u,p})

'b'  K(d[./1]), S(d[./1]) @ 'on_rate'
'u'  K(d[1/.], x{u}), S(d[1/.]) @ 'off_rate_fast'
'u*' K(d[1/.], x{p}), S(d[1/.]) @ 'off_rate_slow'
'p'  K(d[1]), S(d[1], x{u/p}) @ 'phos_rate'

%init: 'C0' K(x{u}), K(x{p}), S(x{u}),
```

In this example, a substrate of kind `S` can be phosphorylated by a kinase of kind `K`. This requires both agents to be bound together. Also, substrate-kinase complexes are more stable when the kinase is phosphorylated itself, which we model by having `off_rate_fast > off_rate_slow`.

### A first query example

The following query prints the current time every time a substrate binds to some kinase:

```
query 'binding-times.csv'
match e:{ S(d[/d.K]) } return time[e]
```

Some remarks:

- The `match` keyword introduces a pattern and the keyword `return` introduces a computation that is executed for every instance of the pattern in the trace.
- Here, the pattern consists in a single event named `e` that obeys the constraint `{ S(d[/d.K]) }`, meaning that event `e` must bind a substrate to a kinase. Kappa's edit notation is used to specify transformations that a trace event must perform to match with `e`.
- Given a specific instance of `e` in the trace, the `time[e]` computation returns the time at which thi instance happened in simulation.
- Executing the query above produces a `binding-times.csv` output file with as many lines as there are matchings of `e` in the trace, each line consisting in a floating point number.

In addition to querying the time for each matched event, we can also query the name of the corresponding rule as follows:

```
query 'bindings.csv' {'binding-time', 'binding-rule'}
match e:{ S(/d[d.K]) } return time[e], rule[e]
```

This query outputs a CSV file with two columns. Note that column names for the CSV output can be specified in curly brackets but doing so is optional. In the rest of this document, we tend to also skip the full `query` header for conciseness.


### Other query examples

To estimate the probability that a substrate is bound to a phosphorylated kinase when it gets phosphorylated itself, one can use the following query:

```
match p:{ S(x{/p}, d[1]), k:K(d[1]) }
return int_state[.p]{k.x}
```

## Reference

## Implementation Details

## Frequently Asked Questions

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

**Remark:** some tests rely on the stability of OCaml's random number generator. The current test suite is optimized to run with OCaml 4.14 and some tests may fail when using OCaml 5 since this version breaks backwards compatibility for the random number generator. When using the test suite, you are therefore encouraged to use OCaml 4.14: `opam switch create ocaml4 4.14.1`.

### Contributing new tests

Users are encouraged to contribute new tests via pull-requests. To add a new test, just add a sub-directory in the `tests` hierarchy and make sure that it works with `runtests.py`.

Here are some tips for debugging and inspectign tests:

- The `runtests.py` script captures the standard output and uses specific KaTie options that must not be changed. To explore running the test with other options or just have the standard output printed on your terminal, you can use the `exec.sh` script instead.
- The `runtests.py` script runs KaTie with a maximal debug level, meaning that a lot of useful files are generated in the `katie-output/debug` directory to help understanding how the trace was processed. In particular, `trace-summary.json` contains a summary of the trace that is friendlier than the original `trace.json` file and `matchings.json` enumerates all found matchings.

In addition to ensuring valid queries are correctly executed, it is also important to ensure that as many invalid queries as possible are detected as such statically (i.e. before they are executed) to avoid having queries failing at runtime, possibly wasting weeks of computation. Thus, users are encouraged to add invalid queries to the tests while including the `errors__` substring in their name. Note that the return code of KaTie can be used to determine whether or not errors were detected statically: a return code of `0` means that all queries ran fine, `1` means that errors were detected statically, `2` means that at least one user error was detected dynamically and `3` indicates an internal error. You may want to file issues when encountering return codes `2` or `3`, even for invalid queries.