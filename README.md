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

We introduce the _trace query language_ and its execution engine KaTie using a simple example involving a substrate-kinase system (`tests/large/catphos` and `tests/unit/catphos-mini`). The example is discussed in more details in the [paper](https://www.cs.cmu.edu/~jlaurent/pdf/papers/cmsb18.pdf).

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

### Single-event queries

#### A first query example

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
match e:{ S(d[/d.K]) }
return time[e], rule[e]
```

This query outputs a CSV file with two columns. Note that column names for the CSV output can be specified in curly brackets but doing so is optional. In the rest of this document, we tend to skip the full `query` header for conciseness.


#### State measures

To estimate the probability that a substrate is bound to a phosphorylated kinase when it gets phosphorylated itself, one can use the following query:

```
match p:{ S(x{/p}, d[1]), k:K(d[1]) }
return int_state[.p]{k.x}
```

As opposed to `time` and `rule` that are event measure (measures associated to a particular event), `int_state` is a _state measure_ (a measure associated to a particular mixture). As a first argument and between square brackets, it takes a _state expression_. For `e` an event identifier, the state expression `.e` refers to the state of the system **before** the triggering of event `e`. Similarly, `e.` refers to the state of the system **after** the triggering of e. As a second argument, `int_state` expects a _site expression_. Here, we build such an expression by introducing a name `k` for the kinase captured in event `p`.

### Multi-event queries

#### Average lifespan of a bond

To estimate the average lifespan of a bond between a kinase and a substrate, we can run the following query:

```
match b:{ s:S(d[./1]), K(d[./1]) }
and first u:{ s:S(d[/.]) } after b
return time[u] - time[b]
```

The pattern in this query matches *two* events `b` and `u`. Event `b` must be a binding event between a substrate and a kinase and event `u` must match the *first* event in the trace after event `b` where the same substrate gets unbound. Note that the constraint according to which the same substrate must be involved in both events is captured by the use of a shared agent variable `s`.

To compute the average lifespan of a bond between a kinase and a substrate conditionned on the fact that the kinase is phosphorylated before the bond breaks, one may be tempted to use the following query:

```
// WRONG
match b:{ s:S(d[./1]), K(d[./1]) }
and first u:{ s:S(d[1/.]), K(d[1/.], x{p}) } after b
return time[u] - time[b]
```

This query is perfectly valid but it does not compute what we want. Indeed, suppose the following events happen in order during simulation:

1. Substrate 1 binds to unphosphorylated Kinase 1
2. Substrate 1 and Kinase 1 unbind
3. Substrate 1 binds to phosphorylated Kinase 2
4. Substrate 1 and Kinase 2 ubind

The trace pattern above would match 1 and 4 together (along with 3 and 4), which is a unwanted. Rather, one can use one of the following queries for example:

```
match b:{ s:S(d[/d.K]) }
and first u:{ s:S(d[/.]) } after b
and u:{ s:S(d[1]), K(d[1], x{p}) }
return time[u] - time[b]
```

```
match b:{ s:S(d[/d.K]) }
and first u:{ s:S(d[1/.]), k:K(d[1/.]) } after b
when int_state[.u]{k.x} = 'p'
return time[u] - time[b]
```

```
match u:{ s:S(d[1/.]), K(d[1/.], x{p}) }
and last b:{ s:S(d[./_]) } before u
return (time[u] - time[b])
```

Some comments:

- The first query adds another clause that additionally constrains `s` to be bound to a phosphorylated kinase in `u`. In general, whenever events are constrained by a `first ... after`  or `last ... before` clause, another clause can be specified for performing additional checks or capturing other agents.
- The second query uses a **`when`-clause** to specify an additional condition for the computation to be performed as a boolean expression.
- The third query uses a `last ... before` clause rather than `first ... after` to avoid the trap altogether.

#### See more

For details on how to run KaTie concretely, you can look at the `exec.sh` example script. All queries mentioned in this tutorial can be found in `tests/unit/catphos-mini/query.katie`. More generally, the test suite is a good resources when it comes to understanding the capabilities and subtelties of the trace query language.

## User Guide

### Semantics of queries

A query is defined by a **trace pattern** along with a **computation**. The trace pattern is a sequence of clauses, each one of them featuring an **event pattern**:

```
<query>          ::=  "match" <trace-pattern> "return" <expr>
<trace-pattern>  ::=  <clause> | <clause> "and" <clause>
<clause>         ::=  <evar> ":" <event-pattern>
                  |   "last" <evar> ":" <event-pattern> "before" <evar>
                  |   "first" <evar> ":" <event-pattern> "after" <evar>
<event-pattern>  ::=  "{" <rule-constr>? <edit-pattern> "}"
```

A trace pattern contains both **agent** and **event** variables. We call **matching** a function that maps every event variable to an event in the trace and every agent variable to an agent in the trace. A trace pattern can be thought as a _predicate_ (i.e. a boolean function) over matchings. For example, consider the following pattern:

```
b:{ s:S(x[./_]) } and first u:{ s:S(x[_/.]) } after b
```

This pattern evaluates to _true_ given a matching `m` if and only if:

1. agent `m(s)` has kind `S`
2. agent `m(s)` has site `x` _free_ before event `m(b)` and _bound_ afterwards
3. agent `m(s)` has site `x` _bound_ before event `m(u)` and _free_ afterwards
4. event `m(u)` is the first event in the trace following `m(b)` for which (3.) is true.

The expression after the `return` keyword is also parametrized my a matching since it can involve both event and agent variables.

**Evaluating a query on a trace consists in executing the provided computation for every choice of a matching `m` that makes the trace pattern true.**

#### An Example

To better understand the semantics of queries as defined above, let us consider a toy Kappa model where an agent `A` can be turned into two agents `B` and an agent `B` into two agents `C` (see `tests/unit/cascade`):

```
'r1' A()-, B()+, B()+  @ 1
'r2' B()-, C()+, C()+  @ 1
```

Simulating those rules on a mixture that starts with a single `A`, the following query admits four valid matchings in total:

```
match e1:{ +a:A }
and first e2: { -a:A, +b:B } after e1
and first e3: { -b:B, +c:C } after e2
return
  rule[e1], time[e1], rule[e2], time[e2], rule[e3], time[e3],
  agent_id{a}, agent_id{b}, agent_id{c}
```

More precisely, on a specific trace corresponding to a specific random seed, the query outputs the following:

```
"_init_", 0, "r1", 1.7935983629643002, "r2", 1.8114148430106074, 0, 2, 3
"_init_", 0, "r1", 1.7935983629643002, "r2", 1.8114148430106074, 0, 2, 4
"_init_", 0, "r1", 1.7935983629643002, "r2", 3.9704521435924418, 0, 1, 5
"_init_", 0, "r1", 1.7935983629643002, "r2", 3.9704521435924418, 0, 1, 6
```

As one can see, all four matchings map `e1` and `e2` to the same event in the trace while two mappings are possible for `e3`. Similarly, there are 1, 2 and 4 possible mappings for agents `a`, `b` and `c` respectively. Note that in this example, the `agent_id` function takes an agent variable as an input and returns a unique global identifier for this agent that identifies it across time. This is in contrast with simluation IDs used in KaSim, where agent IDs can be _recycled_ once agents are deleted.

### Invalid queries

In addition to the semantics discussed above, KaTie places a number of restrictions on the queries that it can evaluate. Queries failing to meet these conditions are rejected statically before they are run:

- **Trace patterns must be _connected_**. One can define the _dependency graph_ of a query as a graph whose nodes are event variables and where there is an edge from `e1` to `e2` if and only if `e2` is constrained by a clause of the form `last e2: {...} before e1` or `first e2: {...} after e1`. A pattern is said to be connected if its dependency graph is. One reason this is a requirement is that non-connected patterns can admit a number of valid matchings that is superlinear in the size of the trace. For example, the pattern `e1:{} and e2:{}` admits $N^2$ valid matchings on a trace with $N$ events. Causal [stories](https://fontana.hms.harvard.edu/sites/fontana.hms.harvard.edu/files/documents/signaling.causality.pdf) are particular instances of connected patterns.
- **The dependency graph of trace patterns must be a _tree_**. Given the previous point, this is equivalent to saying that nodes in the dependency graph have either 0 or 1 predecessors. By connectedness, there has to be exactly one node without predecessors, which we call the **_root event_** of the query. A non-root event `e` must be introduced using a single `first e:... after ...` or `last e:... before ...` clause, which we call the **_defining clause_** for this event while other clauses of the form `e:...` are called **_auxiliary clauses_**. This is not as big a restriction as it may seem. Indeed, patterns such as `last e:... before f and last e:... before g` can be expressed as `last e1:... before f and last e2:... before g when time[e1] = time[e2]` using a [when-clause](#when-clauses). With this trick, KaTie can be used to match arbitrary causal DAGs.
- **All event patterns must be _rooted_**. For every event pattern, the identity of all involved agents must be fully determined by the identity of modified agents. For example, the query `match e:{ s:S(x{p}) } return ...` is invalid since the pattern for `e` is not rooted. Rejecting such a query is once again legitimate since it would produce a huge number of matchings, of the order of $N\times E$ where $N$ is the number of events in the trace and $E$ the total number of agents in the simulation. In contrast, the event pattern `{ s:S(x[u/p], d[1]), k:K(d[1]) }` is rooted since the first agent in the pattern is modified and the identity of the second agent inferrable from a bond. Importantly, this rule is relaxed for event patterns in **auxiliary clauses**. In this case, the identity of all agents in the pattern must be determined by _both_ the identity of its modified agents _and_ the identity of all agents mentioned in the defining clause for the same event. Examples are provided below.

**Note:** the original [paper](https://www.cs.cmu.edu/~jlaurent/pdf/papers/cmsb18.pdf) for the trace query language mentions a much more stringent _rigidity_ requirement that is no longer necessary.

#### Example 1

The following query is invalid since it is not connected:

```
match e1:{ s:S(x{u/p}) } and e2:{ s:S(x{p/u}) } return ...
```

#### Example 2

The following query is an attempt at matching all instances of a substrate getting phosphorylated for the first time on both sites `x` and `y` during the _same_ event:

```
match c:{ +s:S }
and first p:{ s:S(x{u/p}) } after c
and first p:{ s:S(y{u/p}) } after c
return ...
```

However, it is invalid since the non-root event `p` has two defining clauses. The following variation that uses an auxiliary clause is valid but it has a different semantics since it also allows `s` to be phosphorylated and unphosphorylated on site `y` before `p` happens:

```
match c:{ +s:S }
and first p:{ s:S(x{u/p}) } after c
and p:{ s:S(y{u/p}) }
return ...
```

A solution involving a [when-clause](#when-clauses) is:

```
match c:{ +s:S }
and first p1:{ s:S(x{u/p}) } after c
and first p2:{ s:S(y{u/p}) } after c
when time[p1] = time[p2]
return ...
```

#### Example 3

The following query is invalid because the defining pattern for event `e2` is not rooted:

```
match e1:{ s:S(x{u/p}, d[1]), k:K(d[1]) }
and last e2:{ s:S(y{u/p}), k:K(d[.]) } before e1
```

Indeed, agent `k` is not modified in this pattern and its identity cannot be determined by the identity of `s` **within this pattern**, although the identity of `k` _can_ be determined from the identity of `s` in `e1`.

#### Example 4

Finally, coming back to a previous [example](#average-lifespan-of-a-bond), the following query is valid:

```
match b:{ s:S(d[/d.K]) }
and first u:{ s:S(d[/.]) } after b
and u:{ s:S(d[1]), K(d[1], x{p}) }
return time[u] - time[b]
```

This is because despite the auxiliary clause for `u` specifying no agent modification, agent `s` is constrained in the defining clause of `u` and the identity of the kinase in the auxiliary clause is determined by the identity of `s` via a bond.


### Expression language

Computations can be expressed in a small language with the following **types**:

- `int`, `float`: _numerical types_. The standard arithmetic operations (e.g. `+`, `-`) and comparison operators (e.g. `<`, `>=`) are available, along with numerical constants (`0`, `3.14`, `1.3e-7`). Integer are automatically promoted to floating point numbers when doing arithmetic with both types.
- `bool`: _boolean type_. Booleans are printed as `0` or `1` in the tool's CSV output but they are represented using a distinct type internally. Boolean values can be combined using the `&&` and `||` logical operators.
- `string`: _string type_. String literals are delimited by either simple or double quotes.
- `tuple`: _type for tuples of values_. Tuples allow queries to return several results. The comma operator `,` can be used to assemble values into tuples or concatenate tuples together.
- `agent-set`: _type for sets of `(agent_kind, agent_id)` pairs_. Values of this type are returned by some measures such as `component` but cannot be included directly in the query's output. Functions processing agent sets include:
  - `size{s: agent-set} -> int`: size of a set
  - `similarity{s1: agent-set}{s2: agent-set} -> float`: [Jaccard similarity coefficient](https://en.wikipedia.org/wiki/Jaccard_index)
  - `count{kinds: tuple[string]}{s: agent-set} -> tuple[int]`: if `kinds` is a comma-separated list of strings representing agent kinds, this returns a tuple indicating the number of times each agent kind appears in `s`. For example, if `s` contains 3 agents of type A, two agents of type B and four agents of type C, then `count{'B','A'}{s}` yields the tuple `1, 3`.

Some other remarks:

- Equality `=` can be tested between any numerical values or between values of similar type, returning a boolean value.
- An agent variable alone does not define a valid expression (although it can be passed to some [measures](#measures-reference)). To obtain a unique integer identifier from agent variable `a`, one can use the `agent_id{a}` construct. As opposed to IDs used by KaSim, such IDs can be used to compare the identity of different agents across time. The same agent ids are also used in the output of measures such as `snapshot` and `print_cc`.
- A special `null` value is included in the language to be returned as a failure code by measures. Any operation taking `null` as an input must also return `null`, with the exception of equality (e.g. `null = null` is true and `null = 1` is false) and of the comma operator (e.g. `1, null` is a valid tuple).
- Although KaTie's expression language is dynamically typed and type errors can be thrown at runtime, most type errors should be caught statically before queries are executed.

The expression language is not set in stone and can be easily extended. For a summary of currently allowed expressions, one can look at the examples in `tests/unit/expr-basic/query.katie`.


### Measures reference


### Other features

#### Rule constraints

#### When-clauses

#### Every-clauses

### The KaTie CLI


## Implementation Details

### Query evaluation steps

### Event matching algorithm

### Trace-pattern matching algorithm



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