# Ultimate-Tic-Hask-Toe

An [ultimate tic-tac-toe](https://en.wikipedia.org/wiki/Ultimate_tic-tac-toe)
engine, with a minimalistic CLI, written in Haskell. The AI is based on a simple
Monte Carlo Tree Search ([MCTS](https://en.wikipedia.org/wiki/Monte_Carlo_tree_search))
algorithm.

![CLI Preview](/img/Preview.png)

## Setup

To build using [cabal](https://www.haskell.org/cabal/index.html#install-upgrade),
run
```
$ cabal build
```
in the project directory to compile the program, and
```
$ cabal exec ultimate-tic-hask-toe
```
to run it.

## How to

Moves are encoded as 4 digits values, where the first two represent the outer
grid, and the second two represent the inner grid, according to the following
scheme:
```
    00║01║02
    ══╬══╬══
    10║11║12
    ══╬══╬══
    20║21║22
```
hence, to play a move in the outer grid in position 12,
in the slot corresponding to position 01 of the inner grid, the input move
will be `1201`.
The full coordinate chart is the following:
```
    0000|0001|0002║0100|0101|0102║0200|0201|0202
    0010|0011|0012║0110|0111|0112║0210|0211|0212
    0020|0021|0022║0120|0121|0122║0220|0221|0222
    ══════════════╬══════════════╬══════════════
    1000|1001|1002║1100|1101|1102║1200|1201|1202
    1010|1011|1012║1110|1111|1112║1210|1211|1212
    1020|1021|1022║1120|1121|1122║1220|1221|1222
    ══════════════╬══════════════╬══════════════
    2000|2001|2002║2100|2101|2102║2200|2201|2202
    2010|2011|2012║2110|2111|2112║2210|2211|2212
    2020|2021|2022║2120|2121|2122║2220|2221|2222
```

## The engine
The AI is based on an implementation of the MCTS algorithm with a UCB1
selection policy.

### Training

The exploitation parameter has been set by training the AI with a simple
[ELO](https://en.wikipedia.org/wiki/Elo_rating_system)-based script.
The parameter has been chosen using a sensitivity of `0.25`.
