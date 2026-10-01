# tadka-ghc

An adapter for GHC's external `-fdiagnostics-as-json` protocol. It runs
your project's build with GHC's JSON diagnostics protocol always
enabled, decodes GHC's output faithfully, captures anything that isn't
a decodable diagnostic (panics, crashes, non-GHC build-tool failures)
as honestly-labeled data rather than discarding it, and renders the
result through [Tadka](https://github.com/Bombay-Boyz/tadka)'s
diagnostic model.

It is **not** a replacement for GHC's diagnostics, a second diagnostic
renderer, a wrapper around GHC's internal compiler API, an LSP
implementation, or a general logging library. It wraps the external
`cabal`/`stack` build-tool process; GHC itself remains a subprocess
spawned by cabal/stack, never a library dependency of this package.

## Status

All ten phases of the implementation spec are complete and tested. Run
`cabal test` for the current test count (property-based tests, golden
fixtures, and a real-subprocess integration test that builds a scratch
project through a real `cabal`/GHC toolchain are all included). See
[Known limitations](#known-limitations) below for the handful of
honestly-flagged, non-blocking gaps that remain.

## Installation

This package depends on `tadka`, which is not yet on Hackage and is
pulled directly from its GitHub repository. `cabal.project` already
pins an exact commit:

```
source-repository-package
  type:     git
  location: https://github.com/Bombay-Boyz/tadka.git
  tag:      bc750f8694df8424ed0a8c985bd7fa1754dc11ad
```

Build everything:

```bash
cabal build
```

By default, `-Werror` is **off** (a Cabal flag, not hardcoded), so a
future GHC release adding a new warning won't break your install. To
build the way CI does, with warnings promoted to errors:

```bash
cabal build --flags=+werror
```

## Usage

```
tadka-ghc [DIR] [OPTIONS] [-- EXTRA-BUILD-ARGS...]
```

Run it from, or point it at, any `cabal`/`stack` project:

```bash
tadka-ghc                        # build the project in the current directory
tadka-ghc path/to/project        # build a project elsewhere
```

### Options

| Flag | Effect |
|---|---|
| `--cabal` / `--stack` | Force the build tool (default: auto-detected from the project's files -- `stack.yaml` wins if present, otherwise a single `*.cabal` file or `cabal.project`; more than one `*.cabal` file with no `stack.yaml` is reported as ambiguous rather than guessed) |
| `--graphical` / `--narratable` / `--json` | Force the render target (default: Tadka auto-detects the terminal) |
| `--timeout=SECONDS` | Kill the build after this many seconds |
| `--verbose`, `-v` | Show every captured build-tool line, not just decoded diagnostics and (on failure) captured output -- see [Output philosophy](#output-philosophy) |
| `-- ARGS...` | Everything after `--` is passed straight through to the build tool (e.g. `-- --ghc-options=-Wunused-imports`) |
| `--help`, `-h` | Show usage |

The `-fdiagnostics-as-json` flag is injected automatically into every
build -- you never need to pass it yourself, and your own
`--ghc-options` (including an explicit `-fno-diagnostics-as-json`) are
never removed, only appended after, so the flag still wins without
silently overriding anything you wrote.

### Example

```bash
$ tadka-ghc path/to/project-with-a-type-error
```

```
error: Couldn't match expected type 'Char' with actual type 'Int' ...
  +- Main.hs:3:18
  |
3 | main = putStrLn (1 :: Int)
  |                  ^^^^^^^^
```

Decoded diagnostics are bound against the real source file on disk, so
the offending line and the caret underline above are pulled from your
actual project, not synthesized. The process exit code mirrors the
real build outcome (the underlying tool's own exit code on an ordinary
failure; `124` on `--timeout`; `127` if the build tool itself couldn't
be started; `130` if the build process was killed by a signal).

### Output philosophy

Every line a build produces becomes either a faithfully decoded GHC
diagnostic or an honestly-labeled captured record -- nothing is ever
silently dropped. By default, decoded diagnostics are always shown
(a warning matters even on an otherwise successful build), but
captured non-diagnostic output (cabal's own progress text, "Building
executable...", etc.) is only shown when the build did **not**
succeed -- mirroring how `cabal`/`stack` themselves behave: quiet on
success, full context on failure. Pass `--verbose` to always see the
full captured transcript regardless of outcome. This is a
presentation-layer decision only; nothing about what gets captured or
classified changes based on this flag.

## Development

```bash
cabal build --flags=+werror      # strict build, matching CI
cabal test --flags=+werror       # full test suite
cabal check                      # package metadata / distribution sanity
hlint src process app test       # lint
```

### Project layout

- `src/` -- the core library (`tadka-ghc`): pure GHC-JSON decoding,
  semantic promotion, source-span binding, Tadka projection, build-tool
  detection/flag-injection, and output classification. No process I/O.
- `process/` -- the `tadka-ghc-process` component: the one function in
  this package that actually spawns a subprocess (`runBuild`). Kept
  separate so depending on the core library alone never pulls in a
  process-spawning capability you didn't ask for.
- `app/` -- the `tadka-ghc` executable, wiring the above together into
  the CLI described above.
- `test/` -- the test suite: schema/wire-format fixtures, coordinate
  fixtures, golden-rendered output, Hedgehog properties, and a tagged
  real-subprocess integration test.

See `tadka_ghc_vision_final.md` (architecture and design philosophy)
and `tadka_ghc_implementation_spec_final.md` (the phased implementation
plan, including a running record of findings verified against real GHC
behavior and tadka's actual API post-freeze) for the full design
record.

## Known limitations

Documented honestly rather than silently left as gaps:

- **Panic detection covers the common case, not every GHC output
  shape.** A GHC panic that reaches the top-level uncaught-exception
  handler directly is reliably recognized (confirmed against GHC's own
  `GHC.Utils.Panic.Plain` source for the exact installed GHC version
  this project is built against). A panic re-routed through GHC's own
  diagnostic-rendering pipeline -- which can appear as a separate
  `<no location info>: error:` line followed by an indented `panic!`
  line with no `ghc:` prefix -- is not recognized as the start of that
  block. This causes no data loss (every line is still captured), only
  weaker grouping of a multi-line panic into separate records.
- **Schema `"1.2"`'s `rendered` field has never been observed from a
  live GHC.** Its label and required-field shape are confirmed against
  GHC's own source (the commit implementing ticket #26173), but every
  GHC version this project has actually run against so far emits
  schema `"1.1"`. The `test/fixtures/schema/1.2/*` fixtures are
  hand-authored against the confirmed source, not captured from a real
  compiler.

## License

MPL-2.0. See [`LICENSE`](LICENSE) for the full text.
