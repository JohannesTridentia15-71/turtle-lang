# Testing

## How to run the interpreter

```
stack build
stack exec turtle-lang-exe -- <program.rql>
```

The interpreter must be run from the directory containing the `.ttl` files referenced
by the program, because filenames in `.rql` programs are bare (e.g. `foo.ttl`), not
paths.

---

## Manual test cases

Each task has two hand-crafted test cases under `test/taskN/`. Run each case by
`cd`-ing into its directory and executing the interpreter. The expected output is in
`expected.txt` alongside the input files.

```bash
EXE=$(cd /path/to/turtle-lang && stack exec which turtle-lang-exe 2>/dev/null)
(cd test/taskN/base && $EXE tN.rql)
(cd test/taskN/edge && $EXE tN.rql)
```

---

### Task 1 — Graph Union (`test/task1/`)

Query: `combine select from foo.ttl select from bar.ttl`

| Case | 
|------|
| `base/` |
| `medium/` | 
| `edge/` | 

---

### Task 2 — Pattern Matching (`test/task2/`)

Query: `select from baz.ttl where = #predicate <http://example.org/ont/hasAge> and >= #object 21`

| Case | 
|------|
| `base/` | 
| `edge/` | 

---

### Task 3 — Further Pattern Matching (`test/task3/`)

Query: `combine select from qux.ttl where = #predicate <…/studiesAt> and = #object <…/uos> select from qux.ttl where = #predicate <…/worksFor> and = #object <…/uos>`

| Case |
|------|
| `base/` | 
| `edge/` | 

---

### Task 4 — Aggregates (`test/task4/`)

Query: `evaluate max select from quux.ttl`

| Case |
|------|
| `base/` | 
| `edge/` | 

---

### Task 5 — Graph Edits / Transitive Join (`test/task5/`)

Query: `evaluate transitive_join xyzzy.ttl plugh.ttl where #subject = #object`

| Case | 
|------|
| `base/` | 
| `edge/` | 

---

## Automated unit tests (`stack test`)

Run with:

```bash
stack test
```

All tests live in `test/Spec.hs` and use **HUnit**. They exercise pure functions only
(no file I/O). The final line of output is:

```
N tests run, 0 failures, 0 errors.
```

### Lexer (`lexerTests`)

Tests that `alexScanTokens` produces the correct token tag sequence

### Parser (`parserTests`)

Tests that `parseTTL . alexScanTokens` builds the correct AST

### Normalise (`normaliseTests`)

Tests `normaliseTTL` string-in / list-out

### Interpreter (`interpreterTests`)

Tests pure functions against an in-memory `sampleState` containing `people.ttl` and `orgs.ttl`
