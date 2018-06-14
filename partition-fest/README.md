# partition-fest

Given a set of testing outcomes for multiple solutions to the same
problem, this project ranks the solutions according to those
outcomes. The ranking is based on work by Claessen et al.([Cla2010])

## Building, running

Build information is kept in `mkfile`. The default target builds a
SML/NJ heap image, and the whole program can be run can be run using

    sml @SMLload=image

where `image` is the name of the heap image, e.g. `rank-x86.darwin`.

Although older versions may work, the project's been built with:

  - SML/NJ >= 110.82 (target `njbin`; default)
  - MLton >= 20130715 (target `swiss-army-knife`)

## Debugging

SML/NJ can report what functions were active just before an unhandled
exception was raised. To build an image with that support built in,
use the `njdebug` target; the image has the same name as what the
`njbin` target produces.

[Cla2010]: http://www.cse.chalmers.se/~nicsma/papers/ranking-programs.pdf
