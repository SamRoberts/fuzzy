Fuzzy Parser
============

Currently just a fuzzy regex-inspired parser at the moment, but hopefully the
fuzzy family will grow.

This project is still in it's infancy, and in particular, I expect to make big
changes to the pattern language and the cost model used to find the best match.

For example: the current cost model equally penalizes skipping pattern
characters vs. skipping text characters, so if you have a large text and a
relatively small pattern, then the cost of skipping the large text is much
bigger than the cost of skipping the pattern and it makes complete sense to
cram nearly all the text into whatever ".*" parts exist in the pattern,
regardless of whether some of that text matched other parts of the pattern.

The main algorithm appears to be solid enough to work on most inputs you might
want to give it (albeit memory hungry), and given enough memory to create the
required tables can process a 62,000 word book in a matter of seconds.
Presumably there is a lot of performance still on the table.

We haven't yet created a proper executable, but will do so soon. As we do that,
we'll test the program on real world use-cases and tweak the algorithm until it
appears useful.

To do
-----

- Improve practical use of tool based on real world experience:
  - Extend pattern to enable us to give more restricted forms of "variable text"
  - Create a more powerful yaml based pattern representation to enable named matches and other non-regex tools
  - Make Match more detailed and create a yaml based representation for it
- Get benchmarks up and running and then start investigating whether we can
  make code more optimised and safer. Current approach is both overly
  simplistic but also more skeptical of creating new objects than standard
  Scala code.
- Think about how to reduce memory usage. How many cells in early rows in
  table will never be visited by any of the optimal traversals we are tracking
  from later rows?
- Can we support generic monadic parsing rather than just regex inspired
  pattern?  Will require both effiency and a more dynamic replacement for
  tables
- Support for optional reporting of lots of intermediate state in yaml output,
  and visualisation mechanisms for that output. Can we stream output?
- Think about parallelisation. If we had no kleene stars, we could potentially
  process all elements in pattern in parallel, as long as we process from
  (end,end) to (start,start) in a "diagonal-breadth-first" pattern. This would
  require elements on the diagonal to be computed in lock-step and elements at
  step N to be available to computations at step N+1. Not sure how badly the
  precence of kleene stars in pattern interferes with this?

