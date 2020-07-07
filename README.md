Fuzzy Parser
============

Currently just a fuzzy regex inspired parser at the moment, but hopefully the
fuzzy family will grow.

Run fuzzy regex by laughing and moving on to the next tool, until such time as
we actually have a useful main class.

To do
-----

- Use a logical representation of template rather than raw template. This will
  make it easier to do other changes. Probably not realistic to do escaping
  before this.
- Fix out of stack memory issue, I have a bunch of notes about moving to a
  more predictable looping on Pattern's inner method.
- Ye olde split between library and application.
- Get benchmarks up and running and then start investigating whether we can
  make code more optimised and safer. Current approach is both overly
  simplistic but also more skeptical of creating new objects than standard
  Scala code.
- Support escaping within template pattern.
- A lot of hedgehog pattern gen logic might be easier to express with logical
  template representation.
- Once scope instructions are done, consider additional outputs, like printing
  text which matched scope, similar to regex captures. Consider named scopes.
- Can we support generic monadic parsing rather than just regex inspired
  pattern?  will require both effiency and a more dynamic replacement for
  tables
- Support for pluggable visualisation mechanisms, possibly only on inefficient
  algorithm.
- Think about parallelisation. If we had no kleene stars, we could potentially
  process all elements in pattern in parallel, as long as we process from
  (end,end) to (start,start) in a "diagonaal-breadth-first" pattern. This would
  require elements on the diagonal to be computed in lock-step and elements at
  step N to be available to computations at step N+1. Not sure how badly the
  precence of kleene stars in pattern interferes with this?

