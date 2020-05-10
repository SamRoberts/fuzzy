Fuzzy Parser
============

Currently just a fuzzy regex inspired parser at the moment, but hopefully the
fuzzy family will grow.

Run fuzzy regex by laughing and moving on to the next tool, until such time as
we actually have a useful main class.

To do
-----

- Turn into tool which can write matched text and/or report score, and turn
  current main method into proper tests. Attempt Scala hedgehog tests only?
- Ye olde split between library and application.
- Get benchmarks up and running and then start investigating whether we can
  make code more optimised and safer. Current approach is both overly
  simplistic but also more skeptical of creating new objects than standard
  Scala code.
- Use a logical representation of template rather than raw template. This will
  make it easier to do other changes. Probably not realistic to do escaping
  before this.
- Support escaping within template pattern.
- A lot of hedgehog pattern gen logic might be easier to express with logical
  template representation.
- Replace unstructured fork and jump instructions into scope instructions. To
  avoid object allocations (but will have benchmarks to judge if I really
  care), can parse template ahead of time to find out max env depth and
  pre-allocate env arrays.
- Once scope instructions are done, consider additional outputs, like printing
  text which matched scope, similar to regex captures. Consider named scopes.
- Write a simple inefficient algorithm to a) check correctness of complex
  iterative algorithm, and b) help experiment with more powerful parsers
- Prove tableRec is valid and there are no loops. If so, try to get rid of
  stack usage completely, replacing it with a loop that fills out mutable
  tables
- Can we support generic monadic parsing rather than just regex inspired
  pattern?  will require both effiency and a more dynamic replacement for
  tables
- Support for pluggable visualisation mechanisms, possibly only on inefficient
  algorithm, and possibly remove trace from supposedly efficient loop
  algorithm.
- Table evolution over time visualisation.

