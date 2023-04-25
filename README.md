# The Kappa Trace Inquiry Engine

This tool implements a unified language for querying simulation traces of rule-based models written in Kappa about the statistical behavior of individual agents and the relative frequency of different causal patterns. This language was first described in this [paper](https://www.cs.cmu.edu/~jlaurent/pdf/papers/cmsb18.pdf).

- [Installation Instructions](#installation-instructions)
- [Tutorial](#tutorial)
  * [Single-event queries](#single-event-queries)
  * [Multi-event queries](#multi-event-queries)
- [User Guide](#user-guide)
  * [Semantics of queries](#semantics-of-queries)
  * [Invalid queries](#invalid-queries)
  * [Expression language](#expression-language)
  * [Measures reference](#measures-reference)
  * [Other features](#other-features)
    + [Rule constraints](#rule-constraints)
    + [When-clauses](#when-clauses)
    + [Every-clauses](#every-clauses)
  * [The KaTie CLI](#the-katie-cli)
- [Implementation Details](#implementation-details)
- [Testing Instructions](#testing-instructions)
  * [Using the testing script](#using-the-testing-script)
  * [Contributing new tests](#contributing-new-tests)


## Installation Instructions

The simplest way to install KaTie is via opam:

- Install [`opam`](https://opam.ocaml.org/doc/Install.html)
- Install the [`Kappa Tools`](https://github.com/Kappa-Dev/KappaTools): `opam pin add -y https://github.com/Kappa-Dev/KappaTools.git`
- Install KaTie: `opam pin add -y kappa-trace-queries https://github.com/jonathan-laurent/KaTie.git`
- The tool can then be used as `KaTie -t <trace_file> -q <query_file> [options]`

## Tutorial

We introduce the _trace query language_ and its execution engine KaTie using a simple example involving a substrate-kinase system (see `tests/large/catphos` and `tests/unit/catphos-mini`). The example is discussed in more details in the [paper](https://www.cs.cmu.edu/~jlaurent/pdf/papers/cmsb18.pdf).

```
%agent: K(d, x{u,p})
%agent: S(d, x{u,p})

'b'  K(d[./1]), S(d[./1]) @ 'on_rate'
'u'  K(d[1/.], x{u}), S(d[1/.]) @ 'off_rate_fast'
'u*' K(d[1/.], x{p}), S(d[1/.]) @ 'off_rate_slow'
'p'  K(d[1]), S(d[1], x{u/p}) @ 'phos_rate'

%init: 'C0' K(x{u}), K(x{p}), S(x{u})
```

In this example, a substrate of kind `S` can be phosphorylated by a kinase of kind `K`. This requires both agents to be bound together. Also, substrate-kinase complexes are more stable when the kinase is phosphorylated itself, which we model by setting `off_rate_fast` to be much larger than `off_rate_slow`.

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
- Given a specific instance of `e` in the trace, the `time[e]` expression returns the time at which this instance happened in simulation.
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

As opposed to `time` and `rule` that are **_event measures_** (measures associated to a particular event), `int_state` is a **_state measure_** (a measure associated to a particular mixture). As a first argument and between square brackets, it takes a **_state expression_**. For `e` an event identifier, the state expression `.e` refers to the state of the system _before_ the triggering of event `e`. Similarly, `e.` refers to the state of the system _after_ the triggering of e. As a second argument, `int_state` expects a _site expression_. Here, we build such an expression by introducing a name `k` for the kinase captured in event `p`.

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
match b:{ s:S(d[./1]), K(d[./1]) }
and first u:{ s:S(d[1/.]), K(d[1/.], x{p}) } after b
return time[u] - time[b]
```

This query is perfectly valid but it does not compute what we want. Indeed, suppose the following events happen in order during simulation:

1. Substrate 1 binds to unphosphorylated Kinase 1
2. Substrate 1 and Kinase 1 unbind
3. Substrate 1 binds to phosphorylated Kinase 2
4. Substrate 1 and Kinase 2 ubind

The trace pattern above would associate 1 and 4 together (along with 3 and 4 as expected), which is a unwanted. Instead, one can use one of the following queries for example:

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
- The second query uses a [when-clause](#when-clauses) to specify an additional condition for the computation to be performed as a boolean expression.
- The third query uses a `last ... before` clause instead of a `first ... after` clause to avoid the trap altogether.

#### See more

For details on how to run KaTie concretely, you can look at the `exec.sh` example script. All queries mentioned in this tutorial can be found in `tests/unit/catphos-mini/query.katie`. More generally, the [test suite](#testing-instructions) is a good resource when it comes to understanding the capabilities and subtelties of the trace query language.

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
    rule[e1], event_id{e1}, rule[e2], event_id{e2}, rule[e3], event_id{e3},
    agent_id{a}, agent_id{b}, agent_id{c}
```

More precisely, on a specific trace corresponding to a specific random seed, the query outputs the following:

```
"_init_", 0, "r1", 1, "r2", 2, 0, 2, 3
"_init_", 0, "r1", 1, "r2", 2, 0, 2, 4
"_init_", 0, "r1", 1, "r2", 3, 0, 1, 5
"_init_", 0, "r1", 1, "r2", 3, 0, 1, 6
```

As one can see, all four matchings map `e1` and `e2` to the same event in the trace while two mappings are possible for `e3`. Similarly, there are 1, 2 and 4 possible mappings for agents `a`, `b` and `c` respectively. Note that in this example, the `agent_id` function takes an agent variable as an input and returns a unique global identifier for this agent that identifies it across time. This is in contrast with simluation IDs used in KaSim, where agent IDs can be recycled once agents are deleted.

### Invalid queries

In addition to the semantics discussed above, KaTie places a number of restrictions on the queries that it can evaluate. Queries failing to meet these conditions are rejected statically before they are run.

- **Trace patterns must be _connected_**. One can define the _dependency graph_ of a query as a graph whose nodes are event variables and where there is an edge from `e1` to `e2` if and only if `e2` is constrained by a clause of the form `last e2: {...} before e1` or `first e2: {...} after e1`. A pattern is said to be connected if its dependency graph is. One reason this is a requirement is that non-connected patterns can admit a number of valid matchings that is superlinear in the size of the trace. For example, the pattern `e1:{} and e2:{}` admits $N^2$ valid matchings on a trace with $N$ events. Causal [stories](https://fontana.hms.harvard.edu/sites/fontana.hms.harvard.edu/files/documents/signaling.causality.pdf) are particular instances of connected patterns.
- **The dependency graph of trace patterns must be a _tree_**. Given the previous point, this is equivalent to saying that nodes in the dependency graph have either 0 or 1 predecessors. By connectedness, there has to be exactly one node without predecessors, which we call the **_root event_** of the query. A non-root event `e` must be introduced using a single `first e:... after ...` or `last e:... before ...` clause, which we call the **_defining clause_** for this event while other clauses of the form `e:...` are called **_auxiliary clauses_**. This is not as big a restriction as it may seem. Indeed, patterns such as `last e:... before f and last e:... before g` can be expressed as `last e1:... before f and last e2:... before g when event_id{e1} = event_id{e2}` using a [when-clause](#when-clauses). With this trick, KaTie can be used to match arbitrary causal DAGs.
- **All event patterns must be _rooted_**. For every event pattern, the identity of all involved agents must be fully determined by the identity of modified agents. For example, the query `match e:{ s:S(x{p}) } return 1` is invalid since the pattern for `e` is not rooted. Rejecting such a query is once again legitimate since it would produce a huge number of matchings, of the order of $N\times E$ where $N$ is the number of events in the trace and $E$ the total number of agents in the simulation. In contrast, the event pattern `{ s:S(x{u/p}, d[1]), k:K(d[1]) }` is rooted since the first agent in the pattern is modified and the identity of the second agent is inferrable from a bond. Importantly, this rule is relaxed for event patterns in auxiliary clauses. In this case, the identity of all agents in the pattern must be determined by the identity of modified agents _taken together with_ the identity of all agents mentioned in the event's defining clause. Examples are provided below.

**Note:** the original [paper](https://www.cs.cmu.edu/~jlaurent/pdf/papers/cmsb18.pdf) for the trace query language mentions a much more stringent _rigidity_ requirement that is no longer necessary.

<details><summary><b>Example 1: connectedness requirement</b></summary><p>

The following query is invalid since it is not connected:

```
match e1:{ s:S(x{u/p}) } and e2:{ s:S(x{p/u}) } return 1
```

</p></details>

<details><summary><b>Example 2: tree-shaped dependency graph requirement</b></summary><p>

The following query is an attempt at matching all instances of a substrate getting phosphorylated for the first time on both sites `x` and `y` during the _same_ event:

```
match c:{ +s:S }
and first p:{ s:S(x{u/p}) } after c
and first p:{ s:S(y{u/p}) } after c
return 1
```

However, it is invalid since the non-root event `p` has two defining clauses. The following variation that uses an auxiliary clause is valid but it has a different semantics since it also allows `s` to be phosphorylated and unphosphorylated on site `y` before `p` happens:

```
match c:{ +s:S }
and first p:{ s:S(x{u/p}) } after c
and p:{ s:S(y{u/p}) }
return 1
```

A solution involving a [when-clause](#when-clauses) is:

```
match c:{ +s:S }
and first p1:{ s:S(x{u/p}) } after c
and first p2:{ s:S(y{u/p}) } after c
when event_id{p1} = event_id{p2}
return 1
```

</p></details>


<details><summary><b>Example 3: rooted patterns requirement</b></summary><p>

The following query is invalid because the defining pattern for event `e2` is not rooted:

```
match e1:{ s:S(x{u/p}, d[1]), k:K(d[1]) }
and last e2:{ s:S(y{u/p}), k:K(d[.]) } before e1
```

Indeed, agent `k` is not modified in this pattern and its identity cannot be determined from the identity of `s` **within this pattern**, although the identity of `k` _can_ be determined from the identity of `s` in `e1`.

</p></details>


<details><summary><b>Example 4: exception for auxiliary clauses</b></summary><p>

Finally, coming back to a previous [example](#average-lifespan-of-a-bond), the following query is valid:

```
match b:{ s:S(d[/d.K]) }
and first u:{ s:S(d[/.]) } after b
and u:{ s:S(d[1]), K(d[1], x{p}) }
return time[u] - time[b]
```

This is because despite the auxiliary clause for `u` specifying no agent modification, agent `s` is constrained in the defining clause of `u` and the identity of the kinase in the auxiliary clause is determined by the identity of `s` via a bond.

</p></details>


### Expression language

Computations can be expressed in a small language with the following **types**:

- `int`, `float`: _numerical types_. The standard arithmetic operations (e.g. `+`, `-`) and comparison operators (e.g. `<`, `>=`) are available, along with numerical constants (`0`, `3.14`, `1.3e-7`). Integers are automatically promoted to floating point numbers when doing arithmetic with both types.
- `bool`: _boolean type_. Booleans are printed as `0` or `1` in the tool's CSV output but they are represented using a distinct type internally. Boolean values can be combined using the `&&` and `||` logical operators.
- `string`: _string type_. String literals are delimited by either simple or double quotes.
- `tuple`: _type for tuples of values_. Tuples allow queries to return several results. The comma operator `,` can be used to assemble values into tuples or concatenate tuples together.
- `agent_set`: _type for sets of `(agent_kind, agent_id)` pairs_. Values of this type are returned by some measures such as `component` but cannot be included directly in the query's output. Functions processing agent sets include:
  - `size{s: agent_set} -> int`: size of a set
  - `similarity{s1: agent_set}{s2: agent_set} -> float`: [Jaccard similarity coefficient](https://en.wikipedia.org/wiki/Jaccard_index)
  - `count{kinds: tuple[string]}{s: agent_set} -> tuple[int]`: if `kinds` is a comma-separated list of strings representing agent kinds, this returns a tuple indicating the number of times each agent kind appears in `s`. For example, if `s` contains 3 agents of type A, two agents of type B and four agents of type C, then `count{'B','A'}{s}` yields the tuple `1, 3`.

Other remarks:

- **Equality** `=` can be tested between any numerical values or between values of the same type, returning a boolean value.
- An agent variable alone does not define a valid expression (although it can be passed to some [measures](#measures-reference)). To obtain a **unique integer identifier** from agent variable `a`, one can use the `agent_id{a}` construct. As opposed to IDs used by KaSim, such IDs can be used to compare the identity of different agents across time. The same agent IDs are also used in the output of measures such as `snapshot` and `print_cc`.
- Similarly, a **unique event identifier** can be obtained from an event variable `e` using the `event_id{e}` construct. The identifier of an event corresponds to its index in the trace (a trace is defined as a sequence of events). Note that this does not necessarily corresponds to KaSim's `[E]` variable, which does not count initialization events.
- A special `null` value is included in the language to be returned as a **failure code** by measures. Any operation taking `null` as an input must also return `null`, with the exception of equality (e.g. `null = null` is true and `null = 1` is false) and of the comma operator (e.g. `1, null` is a valid tuple).
- Although KaTie's expression language is dynamically typed and type errors can be thrown at runtime, most type errors should be caught statically before queries are executed.

The expression language is not set in stone and can be **easily extended**. For a summary of currently allowed expressions, one can look at the examples in `tests/unit/expr-basic/query.katie`.

### Measures reference

Measures are atomic expressions capturing matching-specific information. They are declined in two kinds: **event measures** and **state measures**. Event measures take as a first argument an event variable between square brackets. In contrast, state measures take as a first argument a **state expression**: for `e` an event variable, `.e` denotes the state _before_ event `e` is triggered and `e.` denotes the state _after_ event `e` is triggered.

**Event measures**:
  - `time[e]: float`: indicates the time of event `e`.
  - `rule[e]: string`: indicates the name of the rule underlying event `e`. Special values `'_init_'`, `'_pert_'` and `'_obs_'` are returned for initial events, perturbation events and observation events respectively. If the rule is un-named, a representation of the transformation is returned. For more details, see the [testing examples](./tests/unit/measure_rule)
  - `debug_event[e]: string`: returns a list of all actions performed by the trace event (e.g. `new(S.0) free(S.0.x) mod(S.0.x, u)`). This is mostly intended for debugging.

**State measures**:
  - `int_state[s]{ag.x}: string`: returns the internal state of site `x` of agent `ag` in state `s`.
  - `component[s]{ag}: agent_set`: returns the connected component of agent `ag` in state `s`.
  - `print_cc[s]{ag}: string`: returns the Kappa graph of the connected component of agent `ag` in state `s`. Unique identifiers are indicated for all agents (i.e. the same identifiers accessible via `agent_id`).
  - `snapshot[s]: string`: performs a snapshot of the full state `s`, stores it into a freshly generated file in JSON format and returns a path to this file. Similarly to `print_cc`, snapshots are annotated with unique agent identifiers. See [here](#when-clauses) for a caveat about using `snapshot` in the presence of when-clauses.

Whenever a measure is called on an agent that does not exist in the specified state, it returns the value `null`.

### Other features

#### Rule constraints

In addition to _edit patterns_, event patterns can feature a rule constraint of the form `'r1'|...|'rn'`, meaning that any matching event must be an instance of one of the listed rules. The special name `'_init_'` can be used to denote an initial event. For example, the following query matches all events `e` that are either initialization events or instances of rule `p` that additionally phosphorylate a substrate that is free on site `d`.

```
match e:{'p'|'_init_' S(x{u/p}, d[.]) }
return ...
```

#### When-clauses

A **when-clause** can be used right before the `return` statement to specify a boolean expression that must be true for the matching to be reported:

```
match e:{ s:S(x[/p]) }
and last b:{ s:S(d[./_]) } before e
when size{component[b.]{s}} >= 3
return snapshot[b.]
```

This query only reports results for which the condition `size{component[b.]{s}} >= 3` evaluates to `true`. Note that when using measures that perform side effects such as `snapshot` in the `return` statement, our implementation provides no guarantee that the measure won't be executed for matchings that fail the when-clause. It only guarantees that the results won't be reported. In the case of snapshots, this may result in more files being created on disk than are referenced in the CSV output. The only case in which we guarantee that only reported measures are computed are for queries with a single event pattern (see [here](#implementation-details) for details).

#### Every-clauses

For queries with only a single event pattern, we additionally allow an **every**-clause to specify a minimal delay between two consecutive matchings:

```
match e:{ S(x{u/p}) }
every 0.2 seconds
when time[e] >= 0 && time[e] <= 10
return snapshot[.e]
```

The query above takes a snapshot before each phosphorylation event, from time 0 to 10, leaving at least 0.2 seconds between each snapshot. Because this query only has a single event pattern, it is additionally guaranteed that no snapshot will be taken and stored on disk that isn't referenced in the query's result.


### The KaTie CLI

The KaTie command-line tool can be run as follow:

```
KaTie -t <trace_file> -q <query_file> [options]
```

Here, `<trace_file>` must be a JSON trace file generated by KaSim using the `-trace` option and `<query>` file must be a file containing a collection of queries to evaluate on the trace. By design, queries are evaluated by batch so as to amortize the cost of streaming through the trace file while recreating intermediate simulation states across queries (see [implementation details](#implementation-details)).

Here are some of KaTie's options:

- `--output-dir`: set the output directory (default: `.`)
- `--debug-level`: set the debug level. Depending on this level, different files are outputted in the `debug` output sub-directory, which are described in the [implementation section](#implementation-details):
  - `0`: output no debug information
  - `1`: only output debug information that can be produced at small runtime cost (default). This includes:
    - `compiled-queries.json`
    - `queries-ast.json`
    - `execution-paths.json`
  - `2`: output all debug information, including the following additional files:
    - `trace-summary.json`
    - `trace-summary-long.json`
    - `event-cache.json`
    - `matchings.json`
    - `measurement-schedule.json`
- `--no-backtraces`: disable exception backtraces (for better performances)
- `--native-snapshots`: dump snapshot using KaSim's native format (`*.ka` instead of `*.json`)
- `--no-color`: disable colors in the output

An example of using KaTie in combination with KaSim can be found in `exec.sh`.


## Implementation Details

One technical challenge of evaluating queries is that queries can refer to intermediate simulation states that are not explicitly represented in the trace file and often impossible to store all at once, even on disk. Queries are thus evaluated by streaming through the trace file, reconstructing intermediate states on the fly via KaSim's `Replay` module while caching a minimal amount of information.

More precisely, KaTie evaluates a query by replaying the trace twice: once for computing all matchings and once for evaluating measures and performing all computations. This is done to avoid the need for performing unnecessary measurements and having to store them in RAM, which can be prohibitively expensive. We detail the multi-step process of evaluating queries below.

### Evaluating queries in five steps

We explain all steps of evaluating a query using a running example. This example can be run using the following command: `python runtests.py tests/unit/double-phos/ run`. We illustrate each step using the debug files produced by KaTie via the `--debug-level 2` option.

<details><summary><b>Kappa model</b></summary><p>

Kappa model used in the example. The model's details are not as important as the trace being produced.

```
%agent: K(d, x{u,p})
%agent: S(d, x{u,p})

'S.K'  K(d[./1]), S(d[./1])  @ 1
'S.S'  S(x[./1], d[1]), S(x[./1], d[2]), K(d[1], x{u}), K(d[2], x{p})  @ 1
'SSp'  S(x[1]{u/p}), S(x[1]{u/p})  @ 1

%init: 4 S(x{u})
%init: 2 K(x{u})
%init: 2 K(x{p})

%mod: |S(x{u})| = 0 do $STOP ;
```

</p></details>

<details><summary><b>Trace file</b></summary><p>

Readable summary of the simulation trace generated by KaSim with random seed 0, from `katie-output/debug/trace-summary.json`:

```json
{
  "0": [ "_init_", "new(S.0) free(S.0.x) mod(S.0.x, u) free(S.0.d)" ],
  "1": [ "_init_", "new(S.1) free(S.1.x) mod(S.1.x, u) free(S.1.d)" ],
  "2": [ "_init_", "new(S.2) free(S.2.x) mod(S.2.x, u) free(S.2.d)" ],
  "3": [ "_init_", "new(S.3) free(S.3.x) mod(S.3.x, u) free(S.3.d)" ],
  "4": [ "_init_", "new(K.4) free(K.4.x) mod(K.4.x, u) free(K.4.d)" ],
  "5": [ "_init_", "new(K.5) free(K.5.x) mod(K.5.x, u) free(K.5.d)" ],
  "6": [ "_init_", "new(K.6) free(K.6.x) mod(K.6.x, p) free(K.6.d)" ],
  "7": [ "_init_", "new(K.7) free(K.7.x) mod(K.7.x, p) free(K.7.d)" ],
  "8": [ "S.K", "bind(S.3.d, K.6.d)" ],
  "9": [ "S.K", "bind(S.1.d, K.5.d)" ],
  "10": [ "S.K", "bind(S.2.d, K.4.d)" ],
  "11": [ "S.K", "bind(S.0.d, K.7.d)" ],
  "12": [ "S.S", "bind(S.3.x, S.2.x)" ],
  "13": [ "SSp", "mod(S.2.x, p) mod(S.3.x, p)" ],
  "14": [ "S.S", "bind(S.0.x, S.1.x)" ],
  "15": [ "SSp", "mod(S.1.x, p) mod(S.0.x, p)" ]
}
```

</p></details>

<details><summary><b>Example query</b></summary><p>

```txt
query 'example.csv' {'p', 'b1', 'b2', 's1', 's2', 'k1', 'k2'}
match p:{ s1:S(x{u/p}), s2:S(x{u/p}) }
and last b1:{ s1:S(d[./1]), k1:K(d[./1]) } before p
and b1:{ k1:K(x{p}) }
and last b2:{ s2:S(d[./1]), k2:K(d[./1]) } before p
return
    event_id{p}, event_id{b1}, event_id{b2},
    agent_id{s1}, agent_id{s2}, agent_id{k1}, agent_id{k2}
```

</p></details>

We now proceed to describe each of the five evaluation steps. Note that only steps 2 and 5 require replaying the trace. When queries are evaluated by batch, the underlying resimulation cost is shared across all queries.

- [1. Compiling the query](#1-compiling-the-query)
- [2. Filling in the event cache](#2-filling-in-the-event-cache)
- [3. Computing matchings](#3-computing-matchings)
- [4. Computing a measurement schedule](#4-computing-a-measurement-schedule)
- [5. Executing the measurement schedule](#5-executing-the-measurement-schedule)

#### 1. Compiling the query

During the compilation step, some checks are performed to ensure that the provided query is [valid](#invalid-queries). Also, an **execution path** is computed, which defines the order in which events in the query are tentatively matched to events in the trace. This order is _topological_, meaning that the root event comes first and any event comes after its predecessors in the query's [dependency graph](#invalid-queries).

For each non-root event, we call **link agent** an agent constrained by both the defining clause of this event and at least one prior event in the execution path. Given these definitions, all matchings of the trace pattern can be enumerated using the following algorithm:

> For every matching of the root event in the trace, determine the identity of all non-root events in the order they appear in the execution path. Given a partial matching `m` of previously constrained events and agents, the only possible matching for an event `e` with defining clause `first e:P after e'` is the first event in the trace after `m(e')` that matches event pattern `P` after constraining the identity of the link agents of `e` with `m` (and similarly for defining clauses of the form `last e:P before e'`).

<details><summary><b>Example</b></summary><p>

The execution path for our example query is summarized as follows in `debug/execution-paths.json`:

```json
{ "example.csv": "p(->s1,s2) b1(s1->k1) b2(s2->k2)" }
```

This means that events `p`, `b1` and `b2` are to be matched in this order. Matching event `p` determines possible mappings for agents `s1` and `s2`. Then, the mapping of event `b1` is determined by the mapping of its unique linked agent `s1`. In turn, matching event `b1` determines possible mappings for agent `k1`. Similarly, the mapping of event `b2` is determined by the mapping of its unique linked agent `s2` and matching event `b2` determines possible mappings for agent `s2`.

</p></details>

#### 2. Filling in the event cache

As previously mentioned, enumerating all the matchings of the trace pattern requires answering questions of the kind: "what is the first event in the trace with index more than $k$ that matches the defining pattern of event $e$, provided a fixed mapping for the link agents of $e$ ?". The second step of evaluating a query consists in replaying the trace to build a data structure capable of efficiently answering such queries. We call this data structure the **event cache**.

The event cache maps any pair $(e, m)$ consisting in an event variable $e$ and a mapping $m$ of the link agents of $e$ to the sequence of _all_ $(i, M')$ pairs such that:

- $i$ is the index of a trace event that matches the defining pattern of $e$ given $m$
- $M'$ is the set of all possible mappings $m'$ for the other agents constrained by $e$ such that trace event $i$ matches _both_ the defining pattern of $e$ and all auxiliary patterns of $e$ given $m$ and $m'$.

The event cache can be filled by performing the following actions, for every index $i$ in the trace and every event variable $e$ with defining pattern $p$ in the query:

- Compute the set of all mappings $m'$ for the agents in $p$ such that $p$ matches trace event $i$ given $m'$:
  - Since trace event $i$ performs a finite (and typically small) number of actions, use those to enumerate possible mappings for the agents modified in $p$.
  - Since $p$ is assumed to be [rooted](#invalid-queries), extend those mappings to cover all agents in $p$.
- Group those mappings according to the values assigned to link agents of $e$. Add one entry to the event cache for each resulting group, taking auxiliary patterns into account.

<details><summary><b>Example</b></summary><p>

The event cache generated for our event query is printed as follows in `debug/event-cache.json`:

```json
{
  "example.csv": {
    "p": {
      "": {
        "13": [ "s1:3 s2:2", "s1:2 s2:3" ],
        "15": [ "s1:0 s2:1", "s1:1 s2:0" ]
      }
    },
    "b1": {
      "s1:0": { "11": [ "k1:7" ] },
      "s1:1": { "9": [] },
      "s1:2": { "10": [] },
      "s1:3": { "8": [ "k1:6" ] }
    },
    "b2": {
      "s2:0": { "11": [ "k2:7" ] },
      "s2:1": { "9": [ "k2:5" ] },
      "s2:2": { "10": [ "k2:4" ] },
      "s2:3": { "8": [ "k2:6" ] }
    }
  }
}
```

Some remarks:
- Since `p` is the root event, it has no link agents. Events `13` and `15` in the trace match the defining pattern of `p`, with two possible agent mappings each time (the defining pattern of `p` is symmetric).
- Potential matchings of event `b1` in the trace are classified according to the resulting mapping of link agent `s1`. The only matching in which `s1` is mapped to agent `0` is with trace event `11` and it maps `k1` to agent `7`.
- The defining pattern of `b1` matches trace events `9` and `10` but the auxiliary pattern fails to match in both cases. This is the reason why the cache features some empty agent mapping lists, which can only occur in the presence of auxiliary patterns.

</p></details>

#### 3. Computing matchings

Once the event cache is filled, all **matchings** of the query's trace pattern can be enumerated without further access to the trace. For every matching of the root event, non-root events are instantiated in the order specified by the execution path, using the current mapping of link agents to index the event cache.

<details><summary><b>Example</b></summary><p>

Our example query admits two matchings, which are summarized in `debug/matchings.json`. Mechanically reconstructing these matchings from the event cache printed in the previous section is left as an exercise to the reader.

```json
{
  "example.csv": [
    "p:13 b1:8 b2:10 | s1:3 s2:2 k1:6 k2:4",
    "p:15 b1:11 b2:9 | s1:0 s2:1 k1:7 k2:5"
  ]
}
```

</p></details>

#### 4. Computing a measurement schedule

Once all matchings are determined, one can compute a **measurement schedule** that specifies the exact order in which measures must be taken and computation results must be printed during a second and final replaying of the trace. The measurement schedule consists in a sequence of $(i, j, k)$ integer triples sorted by increasing value of $i$. Such an $(i, j, k)$ triple can be interpreted as: "while replaying event $i$ from the trace, compute all measures related to event variable number $k$ in matching number $j$".

<details><summary><b>Example</b></summary><p>

The following measurement schedule is printed in `debug/measurement-schedule.json` for our example query:

```json
{
  "example.csv": [
    { "ev_gid": 8, "matching_id": 0, "ev_lid": 1 },
    { "ev_gid": 9, "matching_id": 1, "ev_lid": 2 },
    { "ev_gid": 10, "matching_id": 0, "ev_lid": 2 },
    { "ev_gid": 11, "matching_id": 1, "ev_lid": 1 },
    { "ev_gid": 13, "matching_id": 0, "ev_lid": 0 },
    { "ev_gid": 15, "matching_id": 1, "ev_lid": 0 }
  ]
}
```

Local event ids (`ev_lid`) `0`, `1` and `2` correspond to event variables `p`, `b1` and `b2` respectively. Matchings are indexed in the order they are presented in `debug/matchings.json`.

</p></details>

#### 5. Executing the measurement schedule

Once a measurement schedule is computed, it can be **executed** in order as the trace is replayed a second and final time. For each matching, the query's computation is executed and printed as soon as the last triple associated with this matching is processed, thereby allowing all cached measure values associated with it to be released from memory.

<details><summary><b>Example: final output</b></summary><p>

Our example query outputs the following the CSV file:

```txt
'p', 'b1', 'b2', 's1', 's2', 'k1', 'k2'
13, 8, 10, 3, 2, 6, 4
15, 11, 9, 0, 1, 7, 5
```

</p></details>


### Special case of single-event queries

Compared to _multi-event_ queries, _single-event_ queries are straightforward to evaluate in a single replaying of the trace and without caching _any_ data in memory. Thus, KaTie uses a separate, optimized execution engine for those.


## Testing Instructions

The `tests` directory contains a collection of example models and queries. A subdirectory is considered as defining a valid test if it features a `model.ka` and a `query.katie` file. The `exec.sh` script can be used to run KaSim and KaTie in sequence on a test.

There are two ways to specify the expected outcome of running a test:

1. An `expected/results` folder can be placed in the test directory. The testing script checks that the content of `katie-output` matches the content of `expected/results`.
2. The expected behavior of a query can be encoded in the name of the query itself. If a query's name contains the substring `__matches_N` with `N` a nonnegative integer, then it is expected to produce `N` matches exactly. In addition, if it contains the substring `__all_true`, then every element of the resulting CSV output must be equal to `1`. Finally, if it contains the substring `errors__`, then the query must be detected as invalid **statically** (before execution starts).

### Using the testing script

The testing script `runtests.py` can be used as follows:

```
python runtests.py [TEST_SUBDIRS] {run,clean,diff,promote}.
```

Here, `TEST_SUBDIRS` is a list of sub-directories indicating which tests must be targeted by the command that follows. If no tests are provided, all tests in the `tests` hierarchy are implicitely included. The following commands are available:

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

**Remark:** some tests rely on the stability of OCaml's random number generator. The current test suite is optimized to run with OCaml 4.14 and some tests may fail when using OCaml 5 since this version breaks backwards compatibility for the random number generator. When using the test suite, users are therefore encouraged to use OCaml 4.14: `opam switch create ocaml4 4.14.1`.

### Contributing new tests

Users are encouraged to contribute new tests via pull-requests. To add a new test, just add a sub-directory in the `tests` hierarchy and make sure that it works with `runtests.py`.

Here are some tips for debugging and inspecting tests:

- The `runtests.py` script captures the standard output and uses specific KaTie options that must not be changed. To explore running the test with other options or just have the standard output printed on your terminal, you can use the `exec.sh` script instead.
- The `runtests.py` script runs KaTie with a maximal debug level, meaning that a lot of useful files are generated in the `katie-output/debug` directory to help understanding how the trace was processed. In particular, `trace-summary.json` contains a summary of the trace that is friendlier than the original `trace.json` file and `matchings.json` enumerates all found matchings. Other debug files are discussed in the [implementation section](#implementation-details).

In addition to ensuring that valid queries are correctly executed, it is also important to ensure that invalid queries are detected as such statically (i.e. before they are executed) to avoid having them failing at runtime, possibly wasting weeks of computation. Users are therefore encouraged to add  invalid queries to the test suite with the `errors__` substring included in their name. Note that KaTie's return code can be used to determine whether or not errors were detected statically: a return code of `0` means that all queries ran fine, `1` means that errors were detected statically, `2` means that at least one user error was detected dynamically and `3` indicates an internal error. Users should consider filing issues when encountering return codes `2` or `3`, even for invalid queries.