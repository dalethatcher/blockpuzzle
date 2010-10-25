# blockpuzzle

A 2D sliding block puzzle solver.

## Usage

See puzzle.clj for an example of how to use the program with the classic "Red Donkey" sliding puzzle.  Zeros are 'holes' and numbers are unique for each block.  The end state should be zero for don't care and piece numbers for where the end position is.

Once you've adjusted puzzle.clj, with the puzzle you'd like to solve, run it with:

$ lein uberjar<br/>
$ java -jar java -jar blockpuzzle-1.0.0-SNAPSHOT-standalone.jar

## License

Copyright (C) 2010 Dale Thatcher

Distributed under the Eclipse Public License, the same as Clojure.
