# Xword

**Xword** is a small crossword generator that uses the simulated annealing 
optimisation algorithm. It is written in OCaml with no third-party dependencies.

## Usage

In order the build the program from source, one may use the following command:

```console
$ eval $(opam env) && make
```

Then the `xword` binary can be used as follows:

```
Usage:
	xword [-h <int>] [-w <int>] [-kmax <int>] <path>
Options:
	-h <int>     Set the height of the puzzle (default: 9)
	-w <int>     Set the width of the puzzle (default: 9)
	-kmax <int>  Set the number of iterations of the algorithm (default: 50000)
```

## Example

```console
$ bin/Xword -w 10 -h 10 -kmax 100000 etc/300k-words.txt 
[INFO] Word list loaded into memory
[INFO] k=100000 T=0.00 E=17 (Emin=17)
M _ S D A Y N S _ A 
E X C E L _ A P O S 
A I A _ T U T O R S 
T _ T H O N _ O D E 
S O C _ I T _ N O S 
P U H _ S O C S _ S 
A _ E S T _ H _ B O 
C A S E _ M A N O R 
E R _ C O O L E R S 
_ E C H O I S E S _ 
[INFO] Percentage of empty squares: 17%
```

## Licence

This is free and unencumbered software released into the public domain.
THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND.
For more information, please refer to <http://unlicense.org/>.
