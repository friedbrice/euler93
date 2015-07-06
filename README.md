# euler93

Solution to [Project Euler Problem 93][1] in Haskell.

To run, compile with ghc and execute. It will expect two lines, in
accodance with the spec set forth by shashank21j in [\[2\]][2]: the
first line is ignored, and the second line is a space-seperated list of
positive integers. Output will be (should be) the largest positive
integer such that it and all smaller positive integers are obtainable
from our input list and the four arithmetic operations, or zero if no
positive integer can be generated from our list.

It's broken at the moment, because the algorithm for generating
well-formed arithmetic expressions is non-exhaustive. I'm presently
working on a fix.

Acknowledgements to MichalTerepeta et. al. for the AExpr type in
[\[3\]][3] and to hammer for the algorithm used to generate all abstract
syntax trees in [\[4\]][4].

  [1]: https://projecteuler.net/problem=93
  [2]: https://www.hackerrank.com/contests/projecteuler/challenges/euler093
  [3]: https://wiki.haskell.org/Parsing_a_simple_imperative_language
  [4]: http://stackoverflow.com/questions/9525074/i-need-to-create-haskell-function-which-returns-all-possible-binary-trees-give
