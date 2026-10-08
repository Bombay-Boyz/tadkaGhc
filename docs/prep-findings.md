# Pre-handoff findings: tadka-ghc implementation spec

> **Addendum:** section 7 below was added after inspecting the actual `tadka` source
> (the zip you uploaded, at commit `bc750f8`, tag `v2.0.0.0` — the exact commit
> `tadka-ghc`'s `cabal.project` already pins). It corrects one thing I got wrong from
> the GitHub web page and confirms the rest. Read it before acting on section 4.

I inspected the real repos (`Bombay-Boyz/tadka-ghc`, `Bombay-Boyz/tadka`) and GHC's own
published docs rather than just the spec text. Bottom line: **don't hand this to 5
developers yet.** The spec's compatibility matrix (§15) targets the wrong GHC versions,
the repo's Phase 0 scaffolding is already started but incomplete against the spec's own
non-negotiable bar, and there's no CI yet. All of this is cheap to fix now and expensive
to argue about once people start pushing PRs.

## 1. The GHC support matrix in §15 is wrong — fix this before anyone starts Phase 1

The spec's §15 matrix targets **GHC 9.4.x / 9.6.x / 9.10.x / ≥10.0**. But:

- `tadka-ghc`'s own README (already committed) states the real target is
  **GHC 9.10.3, 9.12.4, 9.14.1** — not 9.4/9.6.
- `tadka` itself (the library tadka-ghc depends on) requires **GHC ≥ 9.10.1** to build
  at all. GHC 9.4.x and 9.6.x are not just untested — they are *impossible* to support,
  because the dependency won't compile there.
- I confirmed from GHC's own shipped schema files that GHC 9.14.1 already emits
  **schema `"1.1"`** (`docs/users_guide/diagnostics-as-json-schema-1_1.json`, present in
  the 9.14.1 docs). It's likely 9.10.x and 9.12.x also emit 1.1, not 1.0 — meaning the
  spec's `RawFieldsV1_0` / `SV1_0` decode path may correspond to **no GHC version this
  project actually supports**. That's fine to keep as dead-but-correct code for
  robustness (the spec deliberately keeps `SchemaVersion` open per §7), but it should be
  a stated decision, not a silent gap someone notices in Phase 9.

**Action:** rewrite §15's matrix rows to 9.10.3 / 9.12.4 / 9.14.1 (plus whatever `≥10.0`
row you still want for forward-compat) before Phase 1 starts. Otherwise developers will
write and test code against a GHC version nobody is going to run.

## 2. One "Blocked" spec item is actually resolved — no fixture collection needed for this one

§15 marks the `"1.2"` schema-version label for the `rendered` field as **provisional /
blocked pending Phase 9 fixture collection**. It doesn't need to be. GHC's own published
docs already settle it, with no compiler run required:

- GHC 10.0.1 (currently the active dev release, per its user's guide) release notes:
  `-fdiagnostics-as-json` now includes the rendered diagnostic message (#26173).
- The actual shipped schema file is named **`diagnostics-as-json-schema-1_2.json`**, and
  I fetched it directly. It confirms, byte for byte:
  - `rendered` is added as an optional `string` field (not required) —
    exactly matching the spec's `rf12Rendered :: Maybe Text` choice.
  - The schema's own comment says: *"rendered" is not a required field so that the
    schema is backward compatible with version 1.1. If you bump the schema version to
    2.0 please also add "rendered" to the required fields.*
  - `additionalProperties: false` is set at the top level — worth noting for Phase 1's
    decoder strictness (an unknown top-level field should be a schema mismatch, not
    silently ignored, if you want to mirror GHC's own contract).

**Action:** mark this row **Verified** in §14/§15, citing the schema file above, instead
of leaving it Blocked. This removes one of the "biggest gated items" without touching a
compiler.

## 3. What still genuinely needs real compiler output (I could not do this from here)

I don't have a Haskell toolchain or network access to GHCup/downloads.haskell.org from
my sandboxed execution environment, only from search/fetch — so I could read GHC's
published schema and release notes, but I could not run `ghc -fdiagnostics-as-json`
myself to capture live fixtures. Still blocked, and still worth doing narrowly before
handoff rather than 5x redundantly:

- **Coordinate conventions** (one-based/code-point/CRLF/tab handling) for real spans
  produced by 9.10.3, 9.12.4, and 9.14.1 specifically — the spec's endpoint convention
  is verified by one hand example, not by real compiler output across the matrix.
- **`isPanicMarker`'s literal prefix** across the three real GHC families — GHC panic
  banner text has changed wording across releases before; don't assume it's constant.
- One captured real panic block per GHC family for `test/fixtures/build/panic/*.txt`.

Concretely: pick one person, install 9.10.3/9.12.4/9.14.1 via ghcup, run a handful of
deliberately-broken snippets through `ghc -fdiagnostics-as-json`, and commit the raw
output as the Phase 9 fixtures. This is an hour of work per version, not a research
project — but it does need an actual toolchain, which I don't have here.

## 4. `tadka` version pinning — mostly already done, one loose end

- `tadka` is **not on Hackage** despite it being described as public — it's GitHub-only
  (`github.com/Bombay-Boyz/tadka`). Worth telling developers this explicitly so nobody
  wastes time searching Hackage.
- `tadka-ghc`'s `cabal.project` **already pins** it via `source-repository-package` to
  commit `bc750f8694df8424ed0a8c985bd7fa1754dc11ad`, and `tadka-ghc.cabal` bounds it as
  `tadka == 2.0.0.0`. So the mechanism from spec §15 is in place — good.
- **Loose end:** `tadka`'s own README describes its current status as "Phase 11 —
  consolidation & v1," tagged `v1.0.0.0`, but the pinned commit's dependent build-depends
  says `2.0.0.0`. Someone should confirm which is current before relying on it — either
  the README is stale or there's a v2 line not reflected in the README. Two-minute check,
  but worth doing before 5 people build against a version number that might not mean
  what the README says it means.
- `tadka` also ships a `tadka:interop-ghc` sub-library (a GHC `SrcSpan` → tadka
  `Span`/`Offset` adapter, confirmed in `tadka-ghc.cabal`'s test-suite dependencies).
  This overlaps in spirit with tadka-ghc's own Phase 3 coordinate conversion, though it
  likely serves a different input (GHC-the-library `SrcSpan` values, not the
  `-fdiagnostics-as-json` line/column integers tadka-ghc parses). Worth having whoever
  owns Phase 3 skim it once, so nobody duplicates work or gets the two conventions
  confused.

## 5. Phase 0 is already started in the repo — and it's missing the spec's own mandatory flags

`tadka-ghc.cabal` already exists with a `common warnings` stanza. It has:

```
-Wall -Wcompat -Wincomplete-uni-patterns -Wincomplete-record-updates
-Wmissing-export-lists -Wmissing-import-lists -Wmissing-signatures
-Wmissing-local-signatures -Wname-shadowing -Wpartial-fields -Widentities
-Wredundant-constraints -Wunused-imports -Wmissing-deriving-strategies
-Wprepositive-qualified-module -Wunused-packages
```

Spec §0.1 mandates `-Wall -Werror -Wincomplete-patterns -Wincomplete-uni-patterns
-Wincomplete-record-updates -Wmissing-fields -Wredundant-constraints` as non-negotiable,
in the `.cabal` file itself. **The committed file is missing `-Werror`,
`-Wincomplete-patterns`, and `-Wmissing-fields`.** Right now the repo will happily accept
a build with warnings — exactly the day-one disagreement your friend wanted to head off.

Also missing against spec Phase 0's deliverables:
- No `executable` stanza yet (spec: one executable, `tadka-ghc`, wrapping Phase 6).
- No `.github/workflows` — CI doesn't exist yet in this repo.

I patched the cabal warnings stanza and added a CI workflow implementing Phase 10's
"two jobs" policy (fake-`BuildRunner` suite on all three matrix GHCs; narrow real-
subprocess integration job on the newest only) against the corrected 9.10.3/9.12.4/9.14.1
matrix. See the attached `ci.yml`. Drop it in as `.github/workflows/ci.yml` and apply the
warnings diff below to `tadka-ghc.cabal`:

```diff
   common warnings
     ghc-options:
       -Wall
+      -Werror
       -Wcompat
       -Wincomplete-uni-patterns
+      -Wincomplete-patterns
       -Wincomplete-record-updates
+      -Wmissing-fields
       -Wmissing-export-lists
```

(I didn't push this — it's a two-line PR someone on your side should open, since I don't
have write access and shouldn't be the one merging to main regardless.)

## 6. Track assignment — Phases 1–5 vs Phase 6

The dependency graph (§11) is real and Phase 6 (build-tool wrapper) genuinely has zero
type-level dependency on Phases 1–5 (decode/projection core) — confirmed by reading
Phase 6.1's types, which reference nothing from Phases 1–5. Both tracks share only
Phase 0. Recommendation, so nobody duplicates scaffolding:

- **Person/pair A — decode & projection core:** Phases 1→2→3→4→5. This is the track
  that depends on the GHC-version and schema decisions above, so it shouldn't start
  writing `RawFieldsV1_0`/etc. until §15's matrix is corrected (item 1).
- **Person/pair B — build-tool wrapper:** Phase 6, then 6.5. Can start immediately after
  Phase 0 lands — doesn't need the matrix fixed first.
- Both converge at **Phase 7** (opaque capture), which depends on Phase 2's
  `decodeDiagnosticLine` and on Phase 6.
- Whoever finishes first should not start Phase 8 (pure stream semantics) alone — it only
  depends on Phase 2, so it can run in parallel with Phase 6/7, and is a good third
  workstream if you have a third person rather than idle time.

Say this out loud to the team before day one, or you'll get two people quietly
scaffolding the same `.cabal` stanzas.

## 7. Verified against the real `tadka` source (the uploaded zip, commit `bc750f8`, v2.0.0.0)

This is the exact commit `tadka-ghc`'s `cabal.project` pins, so everything below is
ground truth for what Phase 1–5 and Phase 10 will actually build against — not a
description of it.

**Correction to item 4 above.** I previously said tadka "requires GHC ≥ 9.10.1,"
taken from text on tadka's GitHub page. That page was showing a stale/cached README —
the actual README at this commit has been rewritten (into a clean Hackage-style
README; the commit log shows exactly that polish pass: "Improve README presentation,"
"Add diagnostic showcase," "Fix README image for Hackage"). The real, current
constraint, from `tadka.cabal` and `.github/workflows/ci.yml` directly:

- `tested-with: GHC ==9.6.7, GHC ==9.8.4, GHC ==9.10.3, GHC ==9.12.4, GHC ==9.14.1`
- tadka's own CI matrix is exactly those five versions.

So tadka does **not** technically block GHC 9.6.x or 9.8.x — it's tested down to
9.6.7. GHC 9.4.x is still out (tadka's floor is 9.6.7, not lower). The reason to use
9.10.3/9.12.4/9.14.1 for tadka-ghc is `tadka-ghc`'s own README choice (a product
decision already made), not a hard wall from `tadka` itself. Worth saying it that way
to the team rather than as "impossible" — someone may reasonably ask why 9.6/9.8
aren't included, and the honest answer is "not chosen," not "can't."

**The version discrepancy from item 4 is resolved, not just noted.** `v2.0.0.0` is
correct and current — confirmed by `tadka.cabal`'s `version:` field, the `CHANGELOG.md`
head ("## 2.0.0.0 — miette-parity hardening"), and the `v2.0.0.0` git tag on this exact
commit. The GitHub-rendered README describing "Phase 11 — v1.0.0.0" was stale content,
not a real second version line. Nothing to reconcile with tadka's maintainer — just
don't trust that cached page.

**The spec's Tadka API assumptions check out, in full, against real source.** I read
`src/Tadka.hs`'s export list and `Tadka.Internal.Diagnostic`'s class definition
directly. Every identifier spec §10 lists as the pinned API is real and exported
exactly as named: `Diagnostic(..)`, `Context(NoContext)`, `Labeled(..)`, `LabelKind`
(with `Primary`/`Secondary` constructors — so `Tadka.Primary` is a real qualified
name, via `LabelKind(..)`'s re-export), `mkNamedSource`, `mkSpan`, `mkContext`,
`Severity(..)` (`SevAdvice`/`SevWarning`/`SevError` — confirming the spec's own note
that GHC's two-valued severity maps onto only two of Tadka's three constructors, with
`SevAdvice` unused, exactly as vision §11 anticipates). The `Diagnostic` class methods
match name-for-name and signature-for-signature: `message`, `context :: e -> Context`
(not `Maybe Context` — spec's I-01 correction was right), `code`, `severity`, `help`,
`url`, `related`, `diagnosticId`, `diagnosticCause`, each with the exact default spec
Phase 5 assumes. **This means Phase 5's design is sound as written; no rework needed
there.**

**`tadka:interop-ghc` exists, and it's *not* a shortcut for Phase 3 — but it is a free
cross-check.** It's a real sub-library (`Tadka.Interop.GHC.spanFromSrcSpan`), but it
converts GHC-the-library's `SrcSpan` (from the `ghc` package, e.g. from a plugin or
GHC-API session) into a tadka `Span`. `tadka-ghc` never has a `SrcSpan` value — it only
ever sees the `{line, column}` integers `-fdiagnostics-as-json` prints. So Phase 3
still has to write its own conversion from scratch; there's no function to import here.

What it *is* good for: **an independent oracle for Phase 9's coordinate property
tests.** `tadka-ghc`'s own test-suite already depends on both `tadka:interop-ghc` and
the `ghc` package (visible in `tadka-ghc.cabal`), which strongly suggests this was the
intended design — take a real source file, get its `SrcSpan` via the GHC API for some
construct, convert it with `Tadka.Interop.GHC.spanFromSrcSpan`, separately run
`-fdiagnostics-as-json` over the same file and decode+convert the JSON span through
Phase 3's own code, and assert the two `Span`s agree. That's a much stronger check than
a hand-picked example, and it's already half-built into the dependency graph. Whoever
owns Phase 3/9 should use it this way rather than only against golden fixtures.

**One concrete disagreement risk between the two conversions, worth deciding now:**
`Tadka.Interop.GHC.offsetFromLineCol` splits source text on `"\n"` only
(`T.splitOn "\n" src`) and does **not** special-case `\r\n` — a Windows-style line
ending's trailing `\r` stays attached to the previous line as ordinary content. Spec
I-07 states tadka-ghc's own Phase 3 *design* deliberately **excludes the CR from the
preceding line's column range**. If Phase 9 cross-checks the two conversions against a
CRLF fixture, they will disagree by one column on affected lines — not because either
is wrong, but because they're solving different problems (one is a generic byte-offset
helper, the other implements tadka-ghc's own stated coordinate convention). Decide now
whether the CRLF property test excludes this comparison, or whether Phase 3 should
document the expected one-column divergence, so it isn't debugged as a bug on the day
someone runs it.

## Summary — what to do before handoff, in order

1. Fix §15's GHC matrix (9.10.3/9.12.4/9.14.1) and mark the `"1.2"` rendered-field row
   Verified. Cheap, unblocks Phase 1's design instead of leaving it against a fictitious
   matrix.
2. ~~Confirm the tadka v1.0.0.0-vs-2.0.0.0 discrepancy~~ — resolved by reading the
   actual source: v2.0.0.0 is correct and current; the GitHub page showing "v1.0.0.0"
   was stale. Nothing to chase here.
3. Land the cabal warnings fix + CI workflow (attached) — a few minutes, stops the
   flag argument before it starts.
4. Assign Phase 1–5 vs Phase 6 explicitly, in writing, before kickoff. Tell whoever
   owns Phase 3/9 to use `tadka:interop-ghc` as an independent oracle for coordinate
   properties (section 7), and to decide up front how the CRLF divergence is handled.
5. Have one person spend an hour with a real 9.10.3/9.12.4/9.14.1 toolchain capturing
   the coordinate and panic-banner fixtures Phase 9 needs — I could not do this part
   myself without a Haskell toolchain and ghcup network access, which this environment
   doesn't have.
