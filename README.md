# squeal-postgresql-qq

This library provides a Template Haskell quasiquoter
parsing SQL as the quoted language and producing corresponding
[`squeal-postgresql`](https://hackage.haskell.org/package/squeal-postgresql)
expressing. The goal is to provide an easier way to use the
[`squeal-postgresql`](https://hackage.haskell.org/package/squeal-postgresql)
library, by eliminating (or at least reducing) the need for the user to
learn the squeal "DSL" and allowing her to write regular SQL instead.

## Development Notes:

Some random notes:

* The status of this library is "very experimental".

* I'm developing against ghc-9.8 currently, so the lower bounds of the
  dependencies reflects that fact. There is no reason why the lower bounds
  can't be relaxed at least back to ghc-9.0.2, but I'm not going to worry that
  until I get a little farther along with this library.

* It is highly unlikely that I'll ever have time to really get 100% complete
  coverage of all possible SQL statements, but I plan to cover at least the
  basics before the first release. However! the plan is to be a little bit lazy
  about it. It isn't particularly difficult to add new supported statement
  structures. If there is something missing you would like to see just add a
  [test](test/test.hs) with the SQL you would like to see work and I'll do my
  best to make it pass.
