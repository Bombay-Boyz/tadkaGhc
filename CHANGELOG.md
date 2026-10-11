# Changelog for tadka-ghc

All notable changes to this package are documented here. The format
follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and
the package follows the [Haskell PVP](https://pvp.haskell.org/).

## 0.1.0.0 -- unreleased

First release.

### Added

- **A faithful decoder for GHC's `-fdiagnostics-as-json` protocol**
  (schema versions 1.0, 1.1 and 1.2), with a typed, total error model:
  malformed input is a value, never an exception.
- **Honest capture of everything else**: panics, crashes and non-GHC
  build-tool output become labelled records, not discarded lines.
- **Source binding** that turns GHC's line/column spans into character
  offsets using GHC's own conventions (verified against real GHC 9.14.1
  output: columns count characters, tabs advance to the next multiple of
  8 plus 1, span ends are exclusive).
- **Package-aware path resolution** for multi-package projects: GHC
  reports package-relative paths, so a path found in more than one
  package is refused rather than guessed, and a note says why no source
  excerpt is shown.
- **Projection into [Tadka](https://hackage.haskell.org/package/tadka)**,
  so its graphical, narratable and JSON renderers handle GHC diagnostics.
- **A `tadka-ghc` executable** that runs `cabal build` or `stack build`
  with the JSON protocol always enabled and renders the result.
  `--timeout` takes a validated number of seconds.
- **`tadka-ghc-process`**, a public sublibrary containing the one
  function that spawns a process, so depending on the core library alone
  never grants that capability.
- **A runner that enforces its timeout.** On POSIX systems the build runs
  in its own process group; on `--timeout`, interrupt, SIGTERM or SIGHUP
  the whole group is stopped (SIGTERM, a short grace period, then
  SIGKILL), so compilers started by the build do not outlive it.
  Reading the build's output is bounded: if a process the build started
  holds the pipes open, reading stops after a grace period and the result
  says so (`CaptureStatus`), keeping everything received, including a
  final line with no newline.
- **Typed timeouts** (`Timeout`, `mkTimeout`): zero, negative, NaN and
  infinite values are unrepresentable.
- **Plain-English renderers** for every error that can reach a terminal;
  no user-facing text is derived from `Show`.

### Supported

- GHC 9.10.3, 9.12.4 and 9.14.1 (the CI matrix) to build this package.
  Projects being built need GHC 9.10 or newer, the first release with
  `-fdiagnostics-as-json`.
- `tadka` 2.0.x.
- The single supported import is `Tadka.GHCProtocol`. Other modules are
  implementation details and may change without a major version bump.
