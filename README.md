# Calculator

This repository contains the source code for a simple CLI desk-type calculator
written in Raku.

I wrote it mostly to see how Raku solves the
[expression problem](https://en.wikipedia.org/wiki/Expression_problem) using
multimethods.

The code also serves as an introductory example of how to implement a lexer, a
recursive descent parser, and a simple tree walking interpreter, all in a couple
hundred lines of clean, documented, well-tested and performant Raku code
(roughly in that order).

The parser code was inspired by [Bob Nystrom](https://github.com/munificent)'s
excellent (and free!) book
"[Crafting Interpreters](https://craftinginterpreters.com)". If you're reading
this, I bet you'll love the book. Be sure to check it out.

## Installation

Simply clone this repo (using, e.g.,
`git clone https://github.com/heyajulia/calculator` or using the green
**Download Code** button on GitHub), and run `main.raku` with a command like
`raku main.raku`.

## Contributing

If you notice a bug, feel free to open an issue. Also, I'm a Raku beginner, so
if you see something that can be done in a better/cleaner/more idiomatic way,
I'm all ears.

My hope is that you will find this code useful, and use it (or parts of it) as a
starting point for more complex interpeters.

## License

The code is licensed under the terms of the
[Artistic License, version 2.0](https://www.perlfoundation.org/artistic-license-20.html).
