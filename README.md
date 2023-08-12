# monkey-interpreter

A simple interpreter for the monkey language described in book "Writing an interpreter in Go".
I may tweak language a little as per I like it to be.

## Goals

- Interpreter should be good enough for reporting errors so should include
  row and col number too.
- Try to be fully functional by utilizing category theoratical concepts whereever possible
- Purity
- Declarative style API for each components

## Contribute?
Well it would be awesome if I get any helps in terms of
- Test cases
- Better abstractions possible
- More mathematical structures
- Look at todos (I would update it regularly)

## TODO
Other than code TODOs:

- Think about how to get more info for UnexpectedEnd ParseError like on which line and column it happened
  - Maybe use 2 time parsing a token strategy for this. First time for checking the same and second for actual parsing.
- Should info interface also contain endRow and endCol?
- is symMapMaybe a classical mathematical structure?
