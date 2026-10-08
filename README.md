# tadka-ghc

[![CI](https://github.com/Bombay-Boyz/tadka-ghc/actions/workflows/ci.yml/badge.svg)](https://github.com/Bombay-Boyz/tadka-ghc/actions/workflows/ci.yml)

An adapter for GHC's external `-fdiagnostics-as-json` protocol. It runs
your project's build with GHC's JSON diagnostics protocol always
enabled, decodes GHC's output faithfully, captures anything that isn't
a decodable diagnostic (panics, crashes, non-GHC build-tool failures)
as honestly-labeled data rather than discarding it, and renders the
result through [Tadka](https://hackage.haskell.org/package/tadka)'s
diagnostic model.

It is **not** a replacement for GHC's diagnostics, a second diagnostic
renderer, a wrapper around GHC's internal compiler API, an LSP
implementation, or a general logging library. It wraps the external
`cabal`/`stack` build-tool process; GHC itself remains a subprocess
spawned by cabal/stack, never a library dependency of this package.

> **Status: pre-release (0.1.0.0, not yet on Hackage).** Known issues
> that are being fixed before release are tracked in
> [`docs/RELEASING.md`](https://github.com/Bombay-Boyz/tadka-ghc/blob/main/docs/RELEASING.md).

## Requirements

- To **build and run** tadka-ghc: GHC 9.10.3, 9.12.4 or 9.14.1 (the CI
  matrix) with `cabal-install` 3.12 or newer. CI runs on Linux.
- The **project you point it at** must be built with GHC 9.10 or newer,
  the first release that has `-fdiagnostics-as-json`.

## Installation

```bash
cabal install tadka-ghc          # the command-line tool
```

Or add it to a project as a library:

```cabal
build-depends: tadka-ghc
```

Everything you need is exported from one module, `Tadka.GHCProtocol`.
The other modules are implementation details and may change without a
major version bump.

The one function that spawns a process lives in a separate public
sublibrary, so depending on the core library alone never grants that
capability. Depend on it only if you want it:

```cabal
build-depends: tadka-ghc, tadka-ghc:tadka-ghc-process
```

By default `-Werror` is **off** (a Cabal flag, not hardcoded), so a
future GHC release adding a new warning won't break your install. To
build the way CI does:

```bash
cabal build --flags=+werror
```

## Command-line usage

```
tadka-ghc [DIR] [--cabal | --stack] [--graphical | --narratable | --json]
          [--timeout SECONDS] [-v|--verbose] [--version] [-- EXTRA-BUILD-ARGS...]
```

```bash
tadka-ghc                        # build the project in the current directory
tadka-ghc path/to/project        # build a project elsewhere
tadka-ghc -- all                 # a multi-package cabal project: pass the target
```

| Option | Effect |
|---|---|
| `--cabal` / `--stack` | Force the build tool (default: auto-detected from the project's files -- `stack.yaml` wins if present, otherwise a single `*.cabal` file or `cabal.project`; more than one `*.cabal` file with no `stack.yaml` is reported as ambiguous rather than guessed). Naming both is a usage error. |
| `--graphical` / `--narratable` / `--json` | Force the render target (default: Tadka auto-detects the terminal). Naming more than one is a usage error. |
| `--timeout SECONDS` | Stop the build after this many seconds. Must be greater than 0 and at most one year; anything else is a usage error. `--timeout=SECONDS` also works. |
| `--verbose`, `-v` | Show every captured build-tool line, not just decoded diagnostics and (on failure) captured output -- see [Output philosophy](#output-philosophy) |
| `-- ARGS...` | Everything after the first `--` is passed straight through to the build tool, including `--help` and `-h` |
| `--version` | Print the version |
| `--help`, `-h` | Show usage |

The `-fdiagnostics-as-json` flag is injected automatically into every
build -- you never need to pass it yourself, and your own
`--ghc-options` (including an explicit `-fno-diagnostics-as-json`) are
never removed, only appended after, so the flag still wins without
silently overriding anything you wrote.

**Exit status** mirrors the real outcome: the build tool's own exit code
on an ordinary failure; `124` if `--timeout` was reached; `127` if the
build tool could not be started; `130` if it was killed by a signal; `2`
for a usage error or a project directory that does not exist.

### Multi-package projects

`cabal build` with no target fails in a project with several packages and
none in the current directory, so pass one: `tadka-ghc -- all`.

GHC reports source paths *relative to the package being built*, so the
same path (`src/Lib.hs`) can name a different file in each package. When
a path matches files in more than one package, tadka-ghc does not guess:
it shows the diagnostic without a source excerpt and prints one note to
standard error saying why. Notes go to standard error, so `--json` output
on standard output stays clean.

### Output philosophy

Every line a build produces becomes either a faithfully decoded GHC
diagnostic or an honestly-labeled captured record. By default, decoded
diagnostics are always shown (a warning matters even on an otherwise
successful build), but captured non-diagnostic output (cabal's own
progress text, "Building executable...", etc.) is only shown when the
build did **not** succeed -- mirroring how `cabal`/`stack` themselves
behave: quiet on success, full context on failure. Pass `--verbose` to
always see the full captured transcript. This is a presentation-layer
decision only; nothing about what gets captured or classified changes
based on this flag.

## Library usage

A complete program that decodes one real GHC diagnostic, binds it to
source text, and renders it. It lives in
[`examples/library-consumer`](https://github.com/Bombay-Boyz/tadka-ghc/tree/main/examples/library-consumer), and CI builds
and runs it on every change, so it cannot go stale.

```haskell
import qualified Tadka
import Tadka.GHCProtocol

main :: IO ()
main = do
  -- 1. Decode. Total: malformed input is a typed error, never a crash.
  diagnostic <- case decodeDiagnosticLine ghcOutputLine of
    Right d -> pure d
    Left e  -> fail ("could not decode: " <> Text.unpack (renderDecodeError e))

  -- 2. Bind the span to real source text. 'projectSourceProvider' reads
  --    files from a project; here the source comes from memory.
  let provider = SourceProvider (\_ -> pure (Right (Just (SourceText sourceText))))
  spanState <- bindSpan provider (ghcSpan diagnostic)

  -- 3. Render through Tadka (graphical, narratable, or JSON).
  Tadka.reportDiagnostic
    (Tadka.withTarget Tadka.TNarratable Tadka.defaultConfig)
    (BoundGhcDiagnostic diagnostic spanState)
```

Its real output (for a diagnostic GHC 9.10.3 printed):

```
Error, Couldn't match type `Int' with `[Char]' Expected: String   Actual: Int In the first argument of `putStrLn', namely `(1 :: Int)' ...
Location: src/Main.hs, line 3, column 18.
Source line 3: "main = putStrLn (1 :: Int)".
The problem is at columns 18 through 25.
GHC version: ghc-9.10.3
GHC code:    83865
```

GHC's own facts (version string, numeric code, span) stay available on
the decoded value, unmodified, through accessors such as `unGhcVersion`
and `unGhcDiagnosticCode`.

## Development

```bash
cabal build --flags=+werror      # strict build, matching CI
cabal test --flags=+werror       # full test suite
cabal check                      # package metadata / distribution sanity
hlint src process cli app test examples   # lint
```

### Project layout

- `src/` -- the core library (`tadka-ghc`): pure GHC-JSON decoding,
  semantic promotion, source-span binding, Tadka projection, build-tool
  detection and flag injection, and output classification.
- `process/` -- the `tadka-ghc-process` sublibrary: the one function that
  actually spawns a subprocess (`runBuild`).
- `cli/` -- command-line parsing, split out so it can be tested without
  spawning the executable and so `optparse-applicative` stays out of the
  core library.
- `app/` -- the `tadka-ghc` executable.
- `test/` -- the test suite: wire-format and coordinate fixtures (several
  captured from real GHC output; see the provenance files), golden
  rendered output, Hedgehog properties, and a tagged real-subprocess
  integration test.
- `examples/` -- a runnable library consumer, built in CI.
- `docs/` -- design record: [`vision.md`](https://github.com/Bombay-Boyz/tadka-ghc/blob/main/docs/vision.md) (architecture and
  philosophy), [`implementation-spec.md`](https://github.com/Bombay-Boyz/tadka-ghc/blob/main/docs/implementation-spec.md),
  [`prep-findings.md`](https://github.com/Bombay-Boyz/tadka-ghc/blob/main/docs/prep-findings.md), and
  [`RELEASING.md`](https://github.com/Bombay-Boyz/tadka-ghc/blob/main/docs/RELEASING.md).

## Known limitations

Documented honestly rather than silently left as gaps:

- **Tab-aware columns have only been verified against GHC 9.14.1.** The
  rule (columns count characters; a tab advances to the next multiple of
  8 plus 1; span ends are exclusive) is confirmed from real output on
  that version. The location header Tadka prints uses its own
  character-based column, so for a line containing a tab it can differ
  from GHC's (`7:6` versus GHC's `7:9`); the underline is correct.
- **Only schema `"1.0"` has been captured from a live GHC** (9.10.3). The
  `1.1` and `1.2` fixtures are hand-authored against GHC's source, not
  captured; the decoder accepts all three.
- **Stack is verified for single-package projects only** (Stack 3.11.1,
  GHC 9.10.3). Multi-package Stack output is untested.
- **Panic grouping is partial.** A GHC panic that reaches the top-level
  uncaught-exception handler is reliably recognized. A panic re-routed
  through GHC's own diagnostic pipeline -- a `<no location info>: error:`
  line followed by an indented `panic!` line -- is not recognized as the
  start of that block. No data is lost (every line is still captured);
  a multi-line panic is just grouped into separate records.
- **Blank lines in failed-build output become empty records.** Cosmetic;
  no information is lost.
- **Linux is what CI exercises.** macOS and Windows are untested.

## License

MPL-2.0. See [`LICENSE`](https://github.com/Bombay-Boyz/tadka-ghc/blob/main/LICENSE) for the full text.
