# N-Puzzle

## A project about solving [N-Puzzles](https://en.wikipedia.org/wiki/15_puzzle) (*with N ≥ 3*)


## Implementation :
This project uses 3 different search algorithm :
- The A* algorithm
- The Greedy Search algorithm
- The Uniform Cost algorithm

And 4 different heuristics :
- The manhanttan distance
- The Eclidian distance
- The diagonal distance
- The Hamming distance

These algorithms are very similar, using the same search function but different cost functions :
- A* uses *h(x) + g(x)*
- Greedy uses *h(x)*
- Uniform uses *g(x)*

With *h(x)* being an admissible heuristic function, and *g(x)* being the cost of the path from the start node.
> Note that the program stops at the first valid solution and won't go for the shortest, as it is an [NP-Hard](http://sumitg.com/assets/n-puzzle.pdf) problem.

## Running the program :
Clone the repo, and simply run :
```bash
stack build
stack run
```
> Make sure that you have installed [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) first.

The program takes a file as argument, being your puzzle, and optionally a custom search and custom heuristic, like so :
```bash
stack run path/to/your/file [optional custom search] [optional custom heuristic]
```
The  input file should be as following :
```
3       # size of your puzzle
0 2 3   # the puzzle itself
1 4 5
8 7 6
```
You should get the following as output :
```
Solving grid using the A* algorithm and the Manhattan distance
0 2 3
1 4 5
8 7 6
-----
1 2 3
0 4 5
8 7 6
-----
1 2 3
8 4 5
0 7 6
-----
1 2 3
8 4 5
7 0 6
-----
1 2 3
8 4 5
7 6 0
-----
1 2 3
8 4 0
7 6 5
-----
1 2 3
8 0 4
7 6 5
-----
Solved with :
- Time complexity  : 6
- Space complexity : 3
```

## Possible improvements :
- Add proper solvability check
- Add more heurstics
- Update data structures for better perfs
- Correct output to get the number of steps properly (current path is too large)
- Add number of moves needed from initial state to final state