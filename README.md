Fuzzy Parser
============

Currently just a fuzzy regex inspired parser at the moment, but hopefully the
fuzzy family will grow.

This project is still in it's very early stages, and I in particular, I fully
expect to make big changes to the pattern language and/or the cost model
associated with imperfect matches.

For example, at the moment, with an equal waiting for skipping a pattern
character vs. skipping a text character, if you have a large text and a
relatively small pattern then the cost of skipping the large text is so much
bigger than the cost of skipping the pattern that it makes complete sense to
cram any random text into whatever .* parts exist in the pattern to avoid the
cost of skipping the large text.

The main algorithm appears to be solid enough to work on most inputs you might
want to give it (albeit memory hungry), and given enough memory to create the
required tables can process a 62,000 word book in a matter of seconds.
Presumably there is a lot of performance still on the table.

We haven't yet created a proper executable, but will do so soon. As we do that
and also test the program on real world use-cases and tweak the algorithm until
it's useful. The program shouldb e ready for version 1 after these things have
happened.

To do
-----

- Ye olde split between library and application. Standardize on API for
  different implementations while I am at it.
- Produce proper CLI program.
- Use on real-world examples and figure out how to make practically useful.
- Improve efficiency of test implementation so we can write larger tests with it.
- Get benchmarks up and running and then start investigating whether we can
  make code more optimised and safer. Current approach is both overly
  simplistic but also more skeptical of creating new objects than standard
  Scala code.
- Think about how to reduce memory usage. How many cells in early rows in
  table will never be visited by any of the optimal traversals we are tracking
  from later rows?
- Evaluate cost of parsing pattern vs. cost of using pattern to parse text.
  Do we need to optimize parsing the pattern?
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

