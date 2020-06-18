# pegi (Parsing Expression Grammar Interpreter)

The **pegi** is an interpreter of [PEG grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar) defined by user.  It parses 
input data and generates the result (parse tree) in format defined 
with **patterns**.  The **patterns** are defined as semantic actions in
grammar description file.

Typically, user gives two filenames: the grammar description file and the data file.
The interpreter reads the descritpion and then uses the grammar to parse input data.
The derivation tree is written down using **patterns** defined in grammar description file.

The following example translates arithmetic expressions into command sequence for arbitraly precision
calculator dc(1) :

*The grammar definition :*

```
digit = '0123456789' $x                  { $x };
number = digit* ["." digit+]             { $digit+ ["." $digit+] };
factor = number $x                       { $x }
       | "(" ws sum ws ")"               { $sum }; 
mul = factor (ws '*/%' $OP ws factor)*   { $factor " " ($factor " " $OP)* };
sum = mul (ws '-+' $OP ws mul)*          { $mul " " ($mul " " $OP)* };
ws = ' \t\r\n'*;
goal = ws sum $x ws !.                   { "9 k " $x " p" };
   ```    

*Input data :*
``` 
  1 + 2 * 3 - 4 
```

*pegi output :*
``` 
 9 k 1  2 3 * +4  - p
```

Current version of **pegi** should be treated as experimental.  It serves as a justification for the idea, 
or - as they say - proof of concept.  Also as a starting point for further experiments.

It was written in Haskell. After some attempts in python and in C++, the Haskell language proved to be the 
most suitable to write and debug programs operating on complex structures.  In line with what its creators claim, 
it supports reasoning about the algorithms on various level of abstraction.

Although **pegi 1.0** is not yet equipped with diagnostic mechanisms, I was able to 
to create some nontricial usage examples.  It was not easy, but not impossible, by any means.
I therefore consider the results of the experiment as satisfactory.

## Installation

The **pegi** is distributed in source form.  It does not use any nonstandard libraries, 
only basic Haskel.  It is not packaged yet, the only dependence is a Haskell compiler.
For reference I attached the Makefile - it utilises standard **ghc** command. 
I think that compilation of **pagi** shoul be straightforward.  

In the **examples/** subdirectory there is a handful of examples.

## Usage

**pegi [ options ] grammar_file [ file ... ] **

Currently there is only one commandline option: -h (help).

The program reads user grammar from the grammar_file and tries to parse the files given as arguments.

## Syntax of grammar description ("metagrammar") 

See pegi.peg file in examples subdirectory for the metagrammar definition.

... to be completed ...

## Future plans

- Error diagnostics - two levels of error messages - metagrammar and user grammar.
- Polish and precise definition of metagrammar - use compound objects as production results
- Rewrite the pegi in another language - C++, Prolog - and compare products
- Package the project (cabal, Stack, hackage)

## License

The pegi parser is distributed under the terms of BSD License (see the LICENSE file for details).

Copyright 2018 Marek Łętowski (letowski@gmail.com) 
