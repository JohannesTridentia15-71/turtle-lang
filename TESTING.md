# Testing

## How to run the tests

```
stack test
```

This compiles the library and test suite, then runs all tests. The final line of output reports how many tests ran and whether any failed or errored, e.g.:

```
42 tests run, 0 failures, 0 errors.
```

## What is tested

### Lexer (`src/Lexer.x`)

### Parser (`src/Parser.y`)

### Normalise (`src/Normalise.hs`)

### Interpreter — pure functions (`src/Interpreter.hs`)
