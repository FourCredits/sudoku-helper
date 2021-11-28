# sudoku-helper

A tool for providing tips to solving a sudoku app. I don't know quite what it
will do yet.

## Terminology

There are three difficult things in computer science: naming things, and off by
one errors. As such, I have used names from
[here](https://www.livesudoku.com/en/tutorial-terminology.php#) throughout the
codebase, rather than reinvent the wheel.

- A row, column, or 3x3 box is referred to as a *house*
- Two cells are *buddies* if they share a house
- *Givens*: cells given at the start that cannot change

## TODOs

- [x] Add `solve` function
- [x] Add `isFinished` function
- [ ] Strong lines for separating squares of 3x3 blocks
- [ ] GUI instead of creating svg images
- [ ] Make it impossible to change givens
- [ ] Can I change it so rows and columns are numbered 1 to 9?
- [ ] Keep track of changes made for solving
- [ ] Keep track of techniques employed for solving
- [x] Use sets rather than lists for notes
- Additional solvers:
  - [x] hidden single
  - [x] naked pair/triple/etc.
  - [x] hidden pair/triple/etc.
  - [ ] x-wing
  - [ ] BUG +1 - should be fairly straightforward
