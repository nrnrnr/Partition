# partition-fest

Given a set of test results for multiple solutions to the same
problem, this project ranks the solutions according to those
results. The ranking is based on work by Claessen et al.([Cla2010])

## Building, running

Build information is kept in `mkfile`. The default target builds a
SML/NJ heap image, and you run the program using that heap image with
the command

    sml @SMLload=image

where `image` is the name of the heap image, e.g. `rank-x86.darwin`.
Building with MLton produces a binary `swiss-army-knife` which accepts
the same arguments and flags as the SML/NJ-based command; that is,
replacing a command's `sml @SMLload=image` with `swiss-army-knife` is
supposed to produce the same behavior.

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
