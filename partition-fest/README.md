# partition-fest

Given a set of test results for multiple solutions to the same
problem, this project ranks the solutions according to those
results. The ranking is based on work by Claessen et al.([Cla2010])

## Building, running

Build information is kept in `mkfile`. And for most cases, the script
`partition` is the easiest way to run the software once it's been
built. If none of the targets are already built then `partition` will
build one for you (thus the `mk` output on first run). If more than
one of the targets has already been built then `partition` picks the
newest one.

If you need to build the project after making changes, you can build
it with either SML/NJ or MLton. There are two SML/NJ targets:
`njdebug` and `njbin`. Both build a heap image `rank.SUFFIX` where
`SUFFIX` is SML/NJ's identifier for your architecture and operating
system. (You can ask for it via `sml @SMLsuffix`.) The debug build
will give you proper call trace information. The MLton target is
`swiss-army-knife`.

Running a specific target's output depends on the target. If you went
the SML/NJ route, then run the resulting heap image with `sml
@SMLload=rank.SUFFIX`. If you used MLton, then running
`swiss-army-knife` is all you need to do.

## Dependencies

Although older versions may work, the project's been built with:

  - SML/NJ >= 110.82 (target `njbin`; default)
  - MLton >= 20130715 (target `swiss-army-knife`)

## Modes

Beyond ranking and partitioning solutions based on test results, this
project also:

  - reports the entropy of an individual test, of the entire
    collection of tests, or of each test individually in the entire
    collection of tests.

  - builds a decision tree based on the mutual entropy of tests.

  - generates a report of how students did on a particular set of
    outcomes. The report contains at most N witnesses of the student's
    test results. The witnesses are drawn in decreasing order of the
    test's mutual information, where mutual information is the entropy
    of a particular test node in the decision tree.

To use a particular mode, supply the mode name after the command,
e.g. `sml @SMLload=image partition`.

### Ranking and partitioning

*Mode name* `partition` (default)

### Entropy

*Mode name* `entropy`

### Decision tree

*Mode name* `decision-tree`

### Student reporting

*Mode name* `report`

## Debugging

SML/NJ can report what functions were active just before an unhandled
exception was raised. To build an image with that support built in,
use the `njdebug` target; the image has the same name as what the
`njbin` target produces.

[Cla2010]: http://www.cse.chalmers.se/~nicsma/papers/ranking-programs.pdf
