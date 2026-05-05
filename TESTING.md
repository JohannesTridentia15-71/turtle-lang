# Testing

## Building and running

```bash
stack build
stack exec turtle-lang-exe -- <program.rql>
```

The interpreter must be invoked from the directory containing the `.ttl` files
referenced by the program, since filenames in `.rql` are bare (e.g. `foo.ttl`).

---

## Automated unit tests

```bash
stack test
```

All tests live in `test/Spec.hs` and use **HUnit**. They cover pure functions only
(no I/O). A passing run ends with:

```
N tests run, 0 failures, 0 errors.
```

| Suite | What is exercised |
|-------|-------------------|
| **Lexer** | Keyword tokens (`select`, `combine`, `evaluate`, `start`, `delete`, …), element markers (`#subject`, `#predicate`, `#object`), comparison operators (`=`, `!=`, `>=`, `<=`, `>`, `<`), `save to` as a single token, file-name and literal tokens. |
| **Parser** | `SQAll`, `CCombine`, `LSaveQuery`, `QDelete`, `QConstruct`, `QAdd`; `where` filters with and without `start`; `evaluate union/max/min/intersection`. |
| **Normalise** | `@prefix` expansion, `@base` expansion, comma-separated object splitting, blank-line stripping, empty input. |
| **Interpreter** | `splitTokens` (space / quoted-string), `parseTurtleFile`, `serializeGraph`, `evalSelect` (full scan and `where` filter with `>=` and `=`), `evalCombine` (merge with deduplication), `evalFilterStart` (`FNot`, `FAnd`). |

---

## Integration tests

Each task has hand-crafted fixtures under `test/taskN/<case>/`. Every fixture
directory contains the input `.ttl` file(s), the `.rql` query, and `expected.txt`.
Run a case with:

```bash
(cd test/taskN/base && stack exec turtle-lang-exe -- tN.rql)
```

Diff against `expected.txt` to verify correctness.

### Task 1 — Graph union (`test/task1/`)

Query: `combine select from foo.ttl select from bar.ttl`

| Case | Scenario |
|------|----------|
| `base/` | Two disjoint graphs; union returns all four triples. |
| `medium/` | Three-triple graph merged with a two-triple graph sharing no subjects. |
| `edge/` | Both graphs share the same subject IRI — duplicate triples must not appear. |

### Task 2 — Predicate/object filter (`test/task2/`)

Query: `select from baz.ttl where = #predicate <…/hasAge> and >= #object 21`

| Case | Scenario |
|------|----------|
| `base/` | Three persons; two satisfy the age threshold. |
| `edge/` | Edge values at the boundary (age = 21 and age = 100) are both included. |

### Task 3 — Combined filter (`test/task3/`)

Query: `combine select … where = #predicate <…/studiesAt> … select … where = #predicate <…/worksFor> …`

| Case | Scenario |
|------|----------|
| `base/` | One student, one employee — one triple each from the two sub-selects. |
| `edge/` | A person satisfies both predicates; the triple appears in both halves of the union. |

### Task 4 — Aggregate max (`test/task4/`)

Query: `evaluate max select from quux.ttl`

| Case | Scenario |
|------|----------|
| `base/` | Two items with numeric prices; only the row with the maximum value is returned. |
| `edge/` | A non-price triple exists; `max` ignores non-numeric objects and still selects the correct row. |

### Task 5 — Transitive join (`test/task5/`)

Query: `evaluate transitive_join xyzzy.ttl plugh.ttl where #subject = #object`

| Case | Scenario |
|------|----------|
| `base/` | Two-hop chain; both Alice and Bob are reachable. |
| `edge/` | The chain has no continuation beyond the second hop — same two results expected. |

### Tasks 6–10 — Pipeline queries

Tasks 6–10 use the `|` pipeline operator to chain statements, intermediate results
saved with `save_to`, and the `in … replace_predicate/object with` transform.

| Task | Feature under test | Key edge cases |
|------|--------------------|----------------|
| **6** (`test/task6/`) | `evaluate difference` on a combined graph | `baz.ttl` is not a subset of the combined graph; `baz.ttl` is empty. |
| **7** (`test/task7/`) | Multi-stage pipeline building a partitioned union via `!=` filters | All triples match the excluded conditions (empty output); every triple matches a different partition. |
| **8** (`test/task8/`) | `replace_predicate` / `replace_object` then `evaluate difference` to find persons who do not know a target | Nobody knows the target (all persons survive); known person is not typed as `foaf:Person`. |
| **9** (`test/task9/`) | `evaluate transitive_join` chained with two `replace_predicate` transforms in a pipeline | No `owl:sameAs` triples in input (empty output); two subjects share a common object. |
| **10** (`test/task10/`) | Multi-file `combine` followed by `in … replace_predicate` to normalise two DC creator variants | Source contains only the newer variant; source contains neither variant (empty output). |

### Lexer (`lexerTests`)

Tests that `alexScanTokens` produces the correct token tag sequence

### Parser (`parserTests`)

Tests that `parseTTL . alexScanTokens` builds the correct AST

### Normalise (`normaliseTests`)

Tests `normaliseTTL` string-in / list-out

### Interpreter (`interpreterTests`)

Tests pure functions against an in-memory `sampleState` containing `people.ttl` and `orgs.ttl`
