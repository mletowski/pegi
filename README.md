# pegi (Parsing Expression Grammar Interpreter)

This program is an interpreter of PEG grammar created by user.
It parses text files and writes the result in the form determined
by the pattern specified in grammar file.

## Installation

This program - the pegi parser/interpreter - is written in Haskell.
It does not depend on any libraries except some standard basic Haskell modules.
It is not packaged yet as stack nor cabal project.
There is simple Makefile prepared, it is possible to compile the code having `ghc` installed and typing the `make` command.

## Usage

>  pegi  [ options ]  grammar_file  [ file ... ]

Currently there is only one commandline option: -h (help).

The program reads user grammar from the *grammar_file* and tries to parse the *files*
given as arguments.

## History

This is the initial release.  The program is still experimental.

## License

The pegi parser is distributed under the terms of BSD License
(see the LICENSE file for details).

Copyright 2018 Marek Łętowski (letowski@gmail.com)


