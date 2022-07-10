# Compile-Time Topological Sorting

This repository presents a solution to compile-time dependency injection problem over pure functions.

Given a set of functions\
<code>{F<sub>i</sub>: (P<sub>i,1</sub>, .., P<sub>i,N<sub>i</sub></sub>) => R<sub>i</sub> | i ∈ 1..N}</code>,\
the functions are sorted in such way `σ` that each type from the set\
<code>{P<sub>σ(i),1</sub>, .., P<sub>σ(i),N<sub>σ(i)</sub></sub>}</code>\
is uniquely covered by a type from the set of return types\
<code>{R<sub>σ(k)</sub> | k ∈ 1..i-1}</code>.

If such an ordering exists, the functions of these types may be combined into a single function\
<code>S: () => (R<sub>1</sub>, .., R<sub>N</sub>)</code>.\
The present implementation combines given functions to an equivalent function with shapeless' heterogeneous list as
return type\
<code>() => R<sub>σ(N)</sub> :: R<sub>σ(N-1)</sub> :: .. :: R<sub>σ(1)</sub> :: HNil</code>\
and allows to retrieve the desired subset of the computed values, ordered arbitrarily.

_Example._ See `TopologicalSortTest.scala`.
