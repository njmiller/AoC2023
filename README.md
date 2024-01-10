Advent of Code 2023 in Ocaml. All problems have been solved using utop. I am just slowly migrating the solutions to a dune project.

This was meant to teach me Ocaml so there are probably ways of doing some problems better (i.e. string parsing vs interating over a list of characters)

# Issues

Code needs to be compiled with "--profile release" because it will give non-exhaustive pattern matching errors if I don't do that.

Some problems which require cycle detection don't "work". I basically output some values to the screen and then manually determined the cycle length and then calculated the solution externally from that.
