# Releasing tadka-ghc

## Not yet released: what blocks publishing 0.1.0.0

Do not press **publish** until every item below is done. Uploading a
*candidate* (see step 6) is always safe: candidates are not public
releases.

| # | Blocker | Why |
|---|---|---|
| 1 | Runner rewrite: audit B1, B2, B3 | A very large build output can stall a non-threaded build; on `--timeout` the compilers started by the build tool can outlive it; a read error silently drops buffered output. |
| 2 | Panic grouping: audit B4 | A panic routed through GHC's diagnostic pipeline is split into separate records. |
| 3 | ~~maintainer, author, copyright~~ | Done: set in `tadka-ghc.cabal`. |
| 4 | User testing signed off (see "User testing" below) | The package has been exercised by its own tests only, not yet by a real project. |
| 5 | CI green on `main` for **all** jobs, on the commit being released | `tested-with` in the cabal file claims GHC 9.10.3, 9.12.4 and 9.14.1. |
| 6 | README "Known limitations" and the CHANGELOG match reality | Remove the limitations fixed above; add any new ones found while testing. |

Evidence still to capture (each narrows a limitation in the README):

- the JSON `version` and `ghcVersion` fields from GHC 9.12.4 and 9.14.1
  (only 9.10.3, schema `1.0`, has been captured from a live GHC);
- tab columns and package-relative paths on GHC 9.10.3 and 9.12.4
  (only 9.14.1 has been captured);
- Stack output for a multi-package project.

`capture-evidence.sh` produces all of this; every step has a time limit.

## Spec amendments owed

The implementation deliberately differs from `docs/implementation-spec.md`
in these places, and the spec should be updated to match:

- `runBuild` and `BuildRunner.execute` take `Maybe Timeout`, not
  `Maybe NominalDiffTime` (sections 6.4 and 9.4).
- A new internal `tadka-ghc-cli` component holds command-line parsing
  (section 29, module layout).
- `InvalidCoordinates` carries a typed `CoordinateError`; `SourceLookupError`
  gained `SourceAmbiguousPath`; `DecodeError` gained `InvalidSpanOrder`.
- The schema table: GHC 9.10.3 emits schema `1.0`, not `1.1`, and its
  `ghcVersion` field reads `ghc-9.10.3`.
- `tadka-ghc-process` is a **public** sublibrary (`visibility: public`).

## Release checklist (every release)

1. CI is green on `main`.
2. Set the new version in `tadka-ghc.cabal` and date the `CHANGELOG.md`
   entry. Follow the [PVP](https://pvp.haskell.org/): bump the first two
   components for an incompatible change to the supported API.
3. Locally:
   ```bash
   cabal build --flags=+werror && cabal test --flags=+werror
   hlint src process cli app test examples
   cabal check
   ```
4. Build the source distribution and test **the tarball**, not the
   checkout (CI's `sdist-roundtrip` job does the same):
   ```bash
   cabal sdist
   mkdir /tmp/sdist-check && tar -xzf dist-newstyle/sdist/tadka-ghc-*.tar.gz -C /tmp/sdist-check
   (cd /tmp/sdist-check/tadka-ghc-* && cabal test --enable-tests)
   ```
5. Check the documentation Hackage will build:
   ```bash
   cabal haddock --haddock-for-hackage
   ```
   Open the generated docs: the home module is `Tadka.GHCProtocol`.
6. Upload a **candidate** (no `--publish`), then check its page and install
   it as a user would:
   ```bash
   cabal upload dist-newstyle/sdist/tadka-ghc-X.Y.Z.W.tar.gz
   ```
7. Publish:
   ```bash
   cabal upload --publish dist-newstyle/sdist/tadka-ghc-X.Y.Z.W.tar.gz
   ```
   If Hackage's own documentation build fails, upload the docs yourself:
   `cabal upload --documentation --publish dist-newstyle/tadka-ghc-X.Y.Z.W-docs.tar.gz`.
8. Tag and push: `git tag vX.Y.Z.W && git push origin vX.Y.Z.W`.

## User testing (before publishing)

The goal is to use the package the way a user would, from the source
distribution rather than from the repository.

1. **The command-line tool.** Install it from the tarball and run it on a
   real project, first with a clean build, then with a deliberate type
   error, a scope error, a warning, and (for a multi-package project) a
   `-- all` build:
   ```bash
   cabal sdist
   cabal install dist-newstyle/sdist/tadka-ghc-*.tar.gz \
     --installdir=/tmp/tadka-bin --install-method=copy --overwrite-policy=always
   /tmp/tadka-bin/tadka-ghc path/to/your/project
   ```
   Compare with plain `cabal build`: every error GHC reports should appear,
   with the right source line and underline.
2. **The library.** In a separate project, depend on the tarball:
   ```
   -- cabal.project
   packages: . /path/to/tadka-ghc-X.Y.Z.W.tar.gz
   ```
   and use `Tadka.GHCProtocol` as in `examples/library-consumer`.
3. Write down anything surprising, even if it is not a bug.
