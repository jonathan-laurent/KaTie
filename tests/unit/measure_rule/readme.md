# Name of a rule

Kappa statements that contain multiple transformation directives can be annotated with one string label. In no case is there a distinction between unary vs. binary rule applications. The behavior of the `rule[e]` measure is as follows:


## Chemical notation rules

If labeled, forward applications use the label, while reverse application append an `_op` (for opposite) to that label.

If unlabeled, forward applications use a `LHS -> RHS` write-out, whereas reverse applications use a `RHS -> LHS` write-out.

<details><summary><b>Example: labeled</b></summary>

Given this molecularily ambiguous statement:

`label A(a[.]), B(b[.]) <-> A(a[.]), B(b[.]) @ binary_rate {unary_rate}, reverse_rate`

Transformation | Context | Printout
---|---|---
`A(a[./1]), B(b[./1])` | binary | `label`
`A(a[./1]), B(b[./1])` | unary | `label`
`A(a[1/.]), B(b[1/.])` | -- | `label_op`

</details>

<details><summary><b>Example: unlabeled</b></summary>

Given this molecularily ambiguous statement:

`A(a[.]), B(b[.]) <-> A(a[.]), B(b[.]) @ binary_rate {unary_rate}, reverse_rate`

Transformation | Context | Printout
---|---|---
`A(a[./1]), B(b[./1])` | binary | `A(a[.]), B(b[.]) -> A(a[1]), B(b[1])`
`A(a[./1]), B(b[./1])` | unary | `A(a[.]), B(b[.]) -> A(a[1]), B(b[1])`
`A(a[1/.]), B(b[1/.])` | -- | `A(a[1]), B(b[1]) -> A(a[.]), B(b[.])`

</details>


## Edit notation rules

If labeled, rule applications will print their label.

If unlabeled, rule applications will use an edit-notation write-out.

<details><summary><b>Example: labeled</b></summary>

Given this molecularily ambiguous statement:

`label A(a[./1]), B(b[./1]) @ binary_rate {unary_rate}`

Transformation | Context | Printout
---|---|---
`A(a[./1]), B(b[./1])` | binary | `label`
`A(a[./1]), B(b[./1])` | unary | `label`

</details>

<details><summary><b>Example: unlabeled</b></summary>

Given this molecularily ambiguous statement:

`A(a[./1]), B(b[./1]) @ binary_rate {unary_rate}`

Transformation | Context | Printout
---|---|---
`A(a[.]), B(b[.]) -> A(a[1]), B(b[1])` | binary | `A(a[./1]), B(b[./1])`
`A(a[.]), B(b[.]) -> A(a[1]), B(b[1])` | unary | `A(a[./1]), B(b[./1])`

</details>

