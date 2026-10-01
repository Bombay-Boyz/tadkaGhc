# Tadka-GHC — Implementation Specification

## 0. Status and How to Read This Document

This document is the implementation specification promised by `tadka_ghc_vision_final.md` (the vision/architecture document, 34 sections). It resolves every item listed in that document's §33 ("Implementation-Spec Boundary") with concrete types, algorithms, and module contents.

Conventions:

- `§N` refers to a section of the vision document.
- `Phase N` refers to a section of this document.
- Where this document makes a decision the vision document left open, that decision is stated explicitly and justified. Nothing here contradicts an explicit rule in the vision document; §14 (Consistency Checklist) proves that by table.
- All Haskell shown is intended to compile as written, modulo import lists, which are omitted for readability.

### 0.1 Non-negotiable engineering bar

Every phase below is held to the same bar, stated once here so it is not repeated forty times:

1. **Totality.** Every function defined in this codebase is total on its declared domain. Partiality is pushed to the boundary and represented as data (`Maybe`, `Either`, a dedicated sum type), never as an exception, an incomplete pattern match, or a sentinel value.
2. **No partial functions, anywhere, including in tests.** `head`, `tail`, `last`, `init`, `(!!)`, `fromJust`, `read`, `error`, `undefined`, and partial record field access are all banned. `-Wall -Werror -Wincomplete-patterns -Wincomplete-uni-patterns -Wincomplete-record-updates -Wmissing-fields -Wredundant-constraints` are mandatory compiler flags on every component (library, executable, test-suite) in the `.cabal` file, not just a CI lint step — a build that does not compile clean under these flags is not a valid build.
3. **Data over control.** Booleans are never used as function parameters to select behaviour ("boolean blindness"); a dedicated sum type is used instead, so call sites are self-describing and pattern matches stay exhaustive under compiler checking.
4. **Smart constructors.** Every type with an invariant narrower than its representation (a `Line` that must be ≥ 1, a `SchemaVersion` drawn from a known set, a `GhcDiagnosticCode` that must be non-negative) is constructed only through a smart constructor returning `Either e a`. Raw constructors are not exported from the public module surface.
5. **Structural style.** Functions are written by case analysis that mirrors the shape of the input type, in the same constructor order the type declares them, so the reader can check exhaustiveness by eye as well as by compiler. Each non-trivial function carries a Haddock comment stating its totality argument in one sentence (e.g. "total by structural recursion on a finite list" or "total: every constructor of `SchemaVersionTag` is handled").

---

## 1. ADTs and GADTs: Where Each Is Used, and Why Not More

The vision calls for schema-specific wire types that cannot accidentally accept fields from the wrong schema version (§8). Ordinary closed ADTs cannot express "this dispatch function's cases are checked exhaustively against a type-level tag" — that requires a GADT. Everywhere else, a plain ADT is both sufficient and clearer, so this codebase contains **exactly one GADT family**: the schema-version-indexed raw wire type and its existential wrapper. Every other type is a plain sum or product type.

This restraint is deliberate. A GADT is justified only when a type-level invariant would otherwise have to be maintained by an unsafe function (an `error "impossible"` branch, an unchecked pattern match, a runtime assertion) — using one anywhere else would add a reader's cognitive cost without removing a real source of unsafety, which works against the "reads like a proof" goal as much as sloppiness would.

```haskell
-- The closed universe of schema versions this implementation understands.
-- Promoted via DataKinds; used only as a type index, never as a value.
data SchemaVersionTag = V1_0 | V1_1 | V1_2
  deriving stock (Eq, Show)

-- Singleton witness for a known schema version. One constructor per tag,
-- so pattern-matching on this type is exhaustively checked by GHC against
-- SchemaVersionTag's constructor set.
data SKnownSchemaVersion (v :: SchemaVersionTag) where
  SV1_0 :: SKnownSchemaVersion 'V1_0
  SV1_1 :: SKnownSchemaVersion 'V1_1
  SV1_2 :: SKnownSchemaVersion 'V1_2

deriving stock instance Show (SKnownSchemaVersion v)
deriving stock instance Eq   (SKnownSchemaVersion v)

-- Schema-indexed raw wire representation (§8: "prefer schema-specific raw
-- representations"). Each constructor carries exactly the fields that exist
-- in that schema version — nothing is a Maybe merely because a later
-- version happens to add it.
data RawDiagnostic (v :: SchemaVersionTag) where
  RawDiagnosticV1_0 :: RawFieldsV1_0 -> RawDiagnostic 'V1_0
  RawDiagnosticV1_1 :: RawFieldsV1_1 -> RawDiagnostic 'V1_1
  RawDiagnosticV1_2 :: RawFieldsV1_2 -> RawDiagnostic 'V1_2

-- Existential: "some raw diagnostic, together with the schema-version
-- witness that proves which constructor of RawDiagnostic it is." This is
-- the only place in the codebase an existential is needed, because it is
-- the only place a runtime value (parsed JSON) must be married to a
-- compile-time-checked dispatch.
data SomeRawDiagnostic where
  SomeRawDiagnostic :: SKnownSchemaVersion v -> RawDiagnostic v -> SomeRawDiagnostic
```

Note carefully what is *not* promoted to this closed, indexed universe: the public `SchemaVersion` type (§7) remains `newtype SchemaVersion = SchemaVersion Text`, exactly as the vision specifies, precisely so that an *unknown* future version (`"1.3"`, `"2.0"`) is still representable as data and can be carried into a rejection error message. `SchemaVersionTag`/`SKnownSchemaVersion` model only the subset of versions this implementation currently supports; classifying a `SchemaVersion` into that closed set is the one function in the whole codebase that is allowed — indeed required — to have a final catch-all case, because the input type is open by design (Phase 3.1).

**Consequence worth stating plainly:** because schema dispatch is exhaustive by construction, this codebase contains **zero calls to `error`, `undefined`, or an "impossible by construction" branch** anywhere in the schema-decoding path. Totality is proven by the type checker, not asserted in a comment.

---

## 2. Module Layout (resolves §33 "Modules")

Matches §29's proposed shape exactly, with one file per module:

```
tadka-ghc.cabal                 -- TWO library components, per §2.9's Core/CLI note below:
                                 -- "tadka-ghc" (the core library) and "tadka-ghc-process"
                                 -- (the process-execution component); plus the "tadka-ghc"
                                 -- executable, which depends on both.
src/                             -- core library ("tadka-ghc"): pure, no process I/O
  Tadka/
    GHCProtocol.hs              -- public re-export surface (Phase 2.6)
    GHCProtocol/
      Types.hs                  -- GhcVersion, GhcSeverity, GhcDiagnosticCode,
                                 -- DiagnosticReason, RenderedDiagnostic,
                                 -- GhcSpan, Line, Column, GhcDiagnostic (Phase 2)
      Schema.hs                 -- SchemaVersion, SchemaVersionTag,
                                 -- SKnownSchemaVersion, RawDiagnostic,
                                 -- SomeRawDiagnostic, classifySchemaVersion (Phase 1)
      Decode.hs                 -- decodeDiagnosticLine, decodeDiagnosticStream,
                                 -- decodeDiagnosticLineWith, DecodingMode,
                                 -- DecodeError, DecodeWarning, per-version Aeson
                                 -- parsers (Phase 2, 5, 8)
      Span.hs                   -- SourceProvider, SourceLookupError, SpanState,
                                 -- SourceBindingError, LineMetadata,
                                 -- computeLineMetadata, coordinateToOffset,
                                 -- convertSpan, bindSpan (Phase 3)
      Diagnostic.hs              -- Tadka.Diagnostic instances for GhcDiagnostic,
                                 -- BoundGhcDiagnostic and OpaqueGhcOutput;
                                 -- BoundGhcDiagnostic itself; messageDoc; helpDoc
                                 -- (Phase 4, 5)
      BuildTool.hs               -- BuildTool, BuildToolDetectionError,
                                 -- OutputStream, detectBuildTool,
                                 -- injectDiagnosticsFlag, FramerState, feedChunk,
                                 -- flushFramer (Phase 6.1-6.3, 6.5 — pure only,
                                 -- no process spawn; see §2.9)
      Opaque.hs                 -- OpaqueGhcOutput, LineClassification,
                                 -- ClassifierState, InterleavedState,
                                 -- classifyStream, classifyInterleaved,
                                 -- BuildOutcome, classifyBuildOutput,
                                 -- attachCompilerResult (Phase 7)
      Process.hs                -- CompilerResult, Signal, ProcessError,
                                 -- BuildResult (Phase 6.1's *types*, re-exported
                                 -- from the core library so BuildOutcome/Opaque.hs
                                 -- can refer to CompilerResult/BuildResult without
                                 -- depending on the process-execution component
                                 -- that actually produces them)
process/                         -- process-execution component ("tadka-ghc-process")
  Tadka/GHCProtocol/
      Runner.hs                 -- runBuild, BuildRunner, ioBuildRunner (Phase 6.4);
                                 -- the library's one genuinely IO-performing,
                                 -- process-spawning module; depends on the core
                                 -- library's Process.hs/BuildTool.hs types but the
                                 -- core library does not depend back on this
app/
  Main.hs                       -- the tadka-ghc executable: wires BuildTool.hs's
                                 -- detection/flag-injection, Runner.hs's runBuild,
                                 -- and Opaque.hs's classification together
                                 -- (Phase 6's CLI entry point)
test/
  fixtures/                     -- Phase 9
  Main.hs
  Schema/, Coordinate/, Projection/, Property/, Build/
```

Internal module contents (the field lists inside `Types.hs`, `Schema.hs`, etc.) are implementation details and may be reorganized without a compatibility commitment, per §29. Only `Tadka.GHCProtocol` (re-exporting the core library's public surface) is the supported import; `Tadka.GHCProtocol.Runner` is a separate, explicitly-named import for callers who want `runBuild` directly rather than through the CLI executable, kept out of `Tadka.GHCProtocol`'s own re-export list so that depending on the core library alone never pulls in a process-spawning capability a caller did not ask for.

**Core/CLI boundary, reconciled against the rewritten `runBuild` (I-35):** the vision separates pure GHC adaptation from process execution; earlier drafts of this specification placed `runBuild` in the same `Build.hs` module, and therefore the same library component, as the pure detection and flag-injection logic, which would have made every consumer of `detectBuildTool`/`injectDiagnosticsFlag` also depend on a process-execution capability whether or not it ever calls `runBuild`. This revision splits what was `Build.hs` into three pieces along exactly that seam: `BuildTool.hs` and `Process.hs` (pure — detection, flag injection, chunk framing, and the `CompilerResult`/`BuildResult` *type* definitions) stay in the core library alongside Phases 1–5 and 7; `Runner.hs` (the actual `IO`-performing spawn/drain/timeout/terminate lifecycle, Phase 6.4) moves to a separate `tadka-ghc-process` component that depends on the core library's types but is not depended on by them. The `tadka-ghc` executable (`app/Main.hs`) is the integration/CLI layer the vision calls for: it is the only place in this package that links both components together. `Opaque.hs`'s `BuildOutcome`/`classifyBuildOutput` (Phase 7.4) consume a `BuildResult` value as pure data — produced by `Runner.hs` in production, or by Phase 9.4's fake `BuildRunner` in tests — so classification itself remains provably free of any process-execution dependency, satisfying the vision's separation without changing any type signature shown elsewhere in this document.

**Dependencies:** core library (`tadka-ghc`): `aeson (>=2.0)`, `text`, `bytestring`, `containers`, `filepath`, `directory`, `prettyprinter`, `tadka` (the library this package adapts to). Process component (`tadka-ghc-process`, I-35): the above plus `typed-process`, since `typed-process` is needed only by `Runner.hs`'s process-spawning `runBuild` and is deliberately not a dependency of the core library at all — a caller who only wants decoding/projection/detection never transitively pulls in a process-execution library. Test-only: `tasty`, `tasty-hedgehog`, `hedgehog`, `tasty-golden`, `tasty-hunit`.

**Mandatory extensions** (set in `default-extensions` of the `.cabal` file, not per-module pragmas, so no module can silently opt out): `GADTs`, `DataKinds`, `KindSignatures`, `StandaloneDeriving`, `StrictData`, `DerivingStrategies`, `DeriveFunctor`, `LambdaCase`, `OverloadedStrings`, `ScopedTypeVariables`, `NamedFieldPuns`, `TypeApplications`, `BangPatterns`, `EmptyDataDecls` (needed for the uninhabited `TadkaConversionError`, Phase 4).

This list is audited against every declaration actually shown in this document, not merely inherited from an earlier draft (I-31): `StandaloneDeriving` is required because §1's `SKnownSchemaVersion` witness derives `Show`/`Eq` via two `deriving stock instance` declarations rather than a `deriving` clause on the data declaration itself — a GADT cannot always derive in-line, which is exactly why those two lines are written as standalone deriving declarations — and the extension enabling that syntax was missing from this list until now. `BangPatterns` is required because `computeLineMetadata` (Phase 3.5) binds its accumulating offset with `!offset` to force it eagerly, part of this document's general strict-accumulator discipline (§12). Every other construct actually used in this document — the schema-indexed GADT and its existential wrapper (`GADTs`, `DataKinds`, `KindSignatures`), `Text.splitOn` (an ordinary `Data.Text` function requiring no extension), record puns (`NamedFieldPuns`), and explicit type application at call sites (`TypeApplications`) — is covered by an extension already declared above; no type or function shown anywhere in this document requires an extension not listed here.

---

## Phase 0 — Scaffolding

**Depends on:** nothing.
**Goal:** a `.cabal` file and empty module skeleton that compiles clean under the mandatory warning flags, with CI wired to reject any change that doesn't.

**Deliverables:**
- `tadka-ghc.cabal` with the library, one executable (`tadka-ghc`, the CLI entry point wrapping Phase 6), and one test-suite, each carrying the warning flags from §0.1.
- Empty stub modules per the Phase 2 layout, each exporting nothing yet, so the module graph exists before any type does.
- A `cabal.project` pinning the exact dependency bounds decided in Phase 10.

**Definition of done:** `cabal build --ghc-options="-Wall -Werror"` succeeds on an empty skeleton; CI (Phase 10) is green on the empty skeleton before any real code is added, so every subsequent phase inherits a working baseline.

---

## Phase 1 — Schema Version and Wire Types (resolves §7 "exact SchemaVersion", §33 "exact Aeson raw types", "schema dispatch")

**Depends on:** Phase 0.
**Vision anchors:** §7 (Supported GHC JSON Schemas), §8 (Schema-Specific Wire Types).

### 1.1 Public, open `SchemaVersion`

```haskell
-- | The protocol version as reported by GHC, kept open (§7: "should not
-- imply that the currently supported versions are the permanent universe").
newtype SchemaVersion = SchemaVersion Text
  deriving stock (Eq, Ord, Show)

mkSchemaVersion :: Text -> SchemaVersion
mkSchemaVersion = SchemaVersion
-- Total and unconditional: any Text is a syntactically valid schema
-- version label, even one we don't support. Support is judged later,
-- by classifySchemaVersion, not here.
```

### 1.2 Classification into the known, closed set

```haskell
data UnsupportedSchemaVersion = UnsupportedSchemaVersion SchemaVersion
  deriving stock (Eq, Show)

data SomeSKnownSchemaVersion where
  SomeSKnownSchemaVersion :: SKnownSchemaVersion v -> SomeSKnownSchemaVersion

-- | Total: every 'SchemaVersion' is either one of the three known labels
-- or falls through to the single, deliberate catch-all — which is the
-- only allowed catch-all in this codebase, justified because
-- 'SchemaVersion' is an intentionally open type (§7).
classifySchemaVersion
  :: SchemaVersion -> Either UnsupportedSchemaVersion SomeSKnownSchemaVersion
classifySchemaVersion sv@(SchemaVersion t) = case t of
  "1.0" -> Right (SomeSKnownSchemaVersion SV1_0)
  "1.1" -> Right (SomeSKnownSchemaVersion SV1_1)
  "1.2" -> Right (SomeSKnownSchemaVersion SV1_2)
  _     -> Left (UnsupportedSchemaVersion sv)
```

This is the mechanism behind §7's dispatch diagram ("1.0 / 1.1 / 1.2 / future version"): "future version" is exactly the wildcard branch above, and it produces data (`UnsupportedSchemaVersion`), never a decode-time crash — satisfying invariant #2 (§32) directly.

### 1.3 Per-version raw field sets

Field names and shapes below are taken directly from GHC's own published schema — `docs/users_guide/diagnostics-as-json-schema-1_1.json` in the GHC source tree — and from a real captured `-fdiagnostics-as-json` line, not inferred. Specifically confirmed:

- top-level fields are exactly `version`, `ghcVersion`, `span`, `severity`, `code`, `message`, `hints`, with `reason` added in 1.1;
- **`severity` is a string enum of exactly `"Warning"` and `"Error"`** — this confirms Phase 2.3's `GhcSeverity` two-constructor design directly from the schema, not merely from the vision's prose;
- **`code` is `integer | null`**, confirming the `Maybe Integer` wire representation already used here;
- **`span` is nested**, not flat: `{ "file": ..., "start": { "line": ..., "column": ... }, "end": { "line": ..., "column": ... } }`, confirmed by a real captured line: `"span":{"file":"<interactive>","start":{"line":2,"column":7},"end":{"line":2,"column":8}}`. This one-character span (`7` to `8` for a single-character token) additionally confirms Phase 3.1's exclusive-end-column assumption directly — it is no longer merely an assumption for that specific point;
- **`reason` (schema 1.1+) is `oneOf` two object shapes**: `{ "flags": [string, ...] }` with `minItems: 1` (confirming the `NonEmpty Text` choice for `RawReasonFlags` directly from the schema's own `minItems` constraint) or `{ "category": string }`.

Not yet independently confirmed against a published schema file, and therefore still **provisional**, pending verification during Phase 9 fixture collection: the exact schema-version number (`"1.2"` vs. a later minor bump) under which the `rendered` field was actually introduced — GHC's 10.0 release notes document a `-fdiagnostics-as-json` change to include the rendered message (ticket #26173), and the schema-versioning convention documented in GHC's own source comments (bump the minor version for a backwards-compatible field addition) makes `"1.2"` the most likely label, but this specification does not treat that specific label as confirmed. §14's checklist marks this row accordingly.

**POST-FREEZE VERIFICATION UPDATE:** confirmed directly against GHC's own source (the commit implementing ticket #26173) rather than inferred. Two findings, not one: (a) `schemaVersion` is indeed bumped to exactly `"1.2"` in the same commit that adds `rendered`, settling the label question stated above. (b) The field is **unconditionally included** in the commit's `jsonDiagnostic` encoder (`("rendered", JSString rendered)`), unlike `code`'s `maybe JSNull ...`-wrapped encoding — meaning `rendered` is **required** under schema 1.2, not optional as this specification originally assumed. That assumption was wrong and has been corrected in the implementation (`RawFieldsV1_2`'s `rf12Rendered` is now `Text`, parsed via `.:`, not `Maybe Text` via `.:?`); see the corresponding correction at Phase 1.3's `RawFieldsV1_2` code block below. This was caught by a decode-time test failure after the correction, confirming the fix is exercised, not merely asserted. Separately, empirically: the actual installed `ghc-9.14.1` binary used to build and test this implementation still emits schema `"1.1"`, not `"1.2"` — that specific binary predates the #26173 commit (merged 2025-07-24) or the change did not make that particular point release. A real GHC emitting `"1.2"` has therefore still never been captured; §15's `GHC ≥10.0` row remains **Blocked** for a fixture, even though the label and shape are now Verified rather than provisional.

Each `RawFieldsVX_Y` record contains only fields that schema believably carries, per the confirmed field set above (`reason` from 1.1; `rendered`, confirmed **required** at 1.2 — see the POST-FREEZE VERIFICATION UPDATE above):

```haskell
data RawFieldsV1_0 = RawFieldsV1_0
  { rf10Version  :: Text            -- GHC compiler version string
  , rf10Span     :: Maybe RawSpan
  , rf10Severity :: Text
  , rf10Code     :: Maybe Integer
  , rf10Message  :: [Text]
  , rf10Hints    :: [Text]
  } deriving stock (Eq, Show)

data RawFieldsV1_1 = RawFieldsV1_1
  { rf11Version  :: Text
  , rf11Span     :: Maybe RawSpan
  , rf11Severity :: Text
  , rf11Code     :: Maybe Integer
  , rf11Message  :: [Text]
  , rf11Hints    :: [Text]
  , rf11Reason   :: Maybe RawReason  -- new in 1.1
  } deriving stock (Eq, Show)

data RawFieldsV1_2 = RawFieldsV1_2
  { rf12Version  :: Text
  , rf12Span     :: Maybe RawSpan
  , rf12Severity :: Text
  , rf12Code     :: Maybe Integer
  , rf12Message  :: [Text]
  , rf12Hints    :: [Text]
  , rf12Reason   :: Maybe RawReason
  , rf12Rendered :: Text             -- CONFIRMED required in 1.2, not optional (POST-FREEZE
                                     -- correction: originally specified as Maybe Text; see note above)
  } deriving stock (Eq, Show)

-- | Nested, per GHC's confirmed wire shape: span -> {file, start, end},
-- each of start/end being its own {line, column} object. Deliberately
-- not flattened into rsStartLine/rsStartCol/rsEndLine/rsEndCol, so the
-- raw type mirrors the wire shape exactly and a nesting-level mistake in
-- the Aeson parser is a type error, not merely a wrong-field-name bug.
data RawSpan = RawSpan
  { rsFile  :: Text
  , rsStart :: RawPosition
  , rsEnd   :: RawPosition
  } deriving stock (Eq, Show)

data RawPosition = RawPosition
  { rpLine   :: Integer
  , rpColumn :: Integer
  } deriving stock (Eq, Show)

-- | Mirrors the schema's oneOf exactly: an object with a non-empty
-- "flags" array, or an object with a "category" string. There is no
-- third wire shape to account for.
data RawReason
  = RawReasonFlags (NonEmpty Text)
  | RawReasonCategory Text
  deriving stock (Eq, Show)
```

`Integer`, not `Int`, is used for every wire-level numeric field, because JSON numbers have no fixed width and a decoder must not silently truncate or overflow on an adversarial or simply large input — narrowing to `Int`/`Word` happens only inside the validated smart constructors of Phase 2, which reject out-of-range values explicitly rather than wrapping them.

### 1.4 Per-version Aeson decoding and dispatch

```haskell
-- | Total per case: exactly one Aeson object-parser per known version,
-- selected by the exhaustively-checked witness.
decodeRawByVersion
  :: SKnownSchemaVersion v -> Aeson.Object -> Either DecodeError (RawDiagnostic v)
decodeRawByVersion SV1_0 obj = RawDiagnosticV1_0 <$> parseFieldsV1_0 obj
decodeRawByVersion SV1_1 obj = RawDiagnosticV1_1 <$> parseFieldsV1_1 obj
decodeRawByVersion SV1_2 obj = RawDiagnosticV1_2 <$> parseFieldsV1_2 obj
```

`parseFieldsV1_0`/`V1_1`/`V1_2` are ordinary total `Aeson.Object -> Either DecodeError RawFieldsVX_Y` functions built from `Aeson`'s `Parser`, converted to `Either` at the boundary (never left as a partial `fromJust`-style unwrap of a `Result`).

**Definition of done:** `decodeRawByVersion` compiles with all three `SKnownSchemaVersion` constructors handled (compiler-checked exhaustiveness — deleting a case is a compile error, not a runtime gap); round-trip fixtures for §9.1's "minimal valid diagnostic" per version parse successfully; a wire object missing a required field for its own version produces `DecodeError`, never a runtime exception.

---

## Phase 2 — Stable Semantic Representation (resolves §33 "exact GhcVersion/GhcSpan/GhcSeverity/GhcDiagnosticCode/DiagnosticReason/RenderedDiagnostic/GhcDiagnostic")

**Depends on:** Phase 1.
**Vision anchors:** §9 (Stable Semantic Representation), §10 (Diagnostic Code), §11 (Severity), §12 (Message Fragments), §13 (Hints), §14 (Reason), §15 (Rendered Diagnostic).

### 2.1 `GhcVersion` — the compiler version, distinct from `SchemaVersion`

```haskell
-- | The GHC compiler version that produced the diagnostic (e.g. "9.10.1").
-- Distinct from 'SchemaVersion' (§9's clarifying note): this identifies the
-- compiler, not the JSON protocol revision.
newtype GhcVersion = GhcVersion Text
  deriving stock (Eq, Ord, Show)

mkGhcVersion :: Text -> Either DecodeError GhcVersion
mkGhcVersion t
  | Text.null t = Left (InvalidGhcVersion t)
  | otherwise   = Right (GhcVersion t)
```

### 2.2 `GhcDiagnosticCode`

```haskell
-- | Preserves GHC's numeric code exactly (§10). Never fabricated into a
-- Tadka DiagnosticCode.
newtype GhcDiagnosticCode = GhcDiagnosticCode Int
  deriving stock (Eq, Ord, Show)

mkGhcDiagnosticCode :: Integer -> Either DecodeError GhcDiagnosticCode
mkGhcDiagnosticCode n
  | n < 0                                  = Left (InvalidDiagnosticCode n)
  | n > toInteger (maxBound :: Int)        = Left (InvalidDiagnosticCode n)
  | otherwise                              = Right (GhcDiagnosticCode (fromInteger n))
```

### 2.3 `GhcSeverity`

```haskell
-- | Exactly the two values GHC's schema defines (§11). No Advice case —
-- inventing one would violate §11 directly.
data GhcSeverity = SevWarning | SevError
  deriving stock (Eq, Show)

-- | Total: any wire string that isn't exactly "Warning" or "Error" is
-- rejected explicitly, consistent with the fail-fast rule §11 added for
-- unrecognized severities.
mkGhcSeverity :: Text -> Either DecodeError GhcSeverity
mkGhcSeverity "Warning" = Right SevWarning
mkGhcSeverity "Error"   = Right SevError
mkGhcSeverity other     = Left (UnrecognizedSeverity other)
```

### 2.4 `DiagnosticReason` and `RenderedDiagnostic`

Reused verbatim from the vision's own sketch (§14) — no change needed, since it is already a closed, total ADT:

```haskell
data DiagnosticReason
  = ReasonFlags (NonEmpty Text)
  | ReasonCategory Text
  deriving stock (Eq, Show)

newtype RenderedDiagnostic = RenderedDiagnostic Text
  deriving stock (Eq, Show)
```

### 2.5 `GhcDiagnostic`

Field types and names are exactly those in §9; nothing is renamed or restructured, because that struct is the contract other phases and the vision document both depend on:

```haskell
data GhcDiagnostic = GhcDiagnostic
  { ghcVersion   :: GhcVersion
  , ghcSpan      :: Maybe GhcSpan          -- Phase 3
  , ghcSeverity  :: GhcSeverity
  , ghcCode      :: Maybe GhcDiagnosticCode
  , ghcMessage   :: [Text]                 -- deliberately not NonEmpty: §12
                                            -- forbids rejecting an empty
                                            -- message array
  , ghcHints     :: [Text]                 -- deliberately not NonEmpty,
                                            -- same reasoning as ghcMessage
  , ghcReason    :: Maybe DiagnosticReason
  , ghcRendered  :: Maybe RenderedDiagnostic
  } deriving stock (Eq, Show)
```

`ghcMessage`/`ghcHints` are intentionally left as plain lists, not tightened to `NonEmpty Text`, even though "avoid partial functions" might tempt a reader toward `NonEmpty` everywhere. Doing so here would silently reject a legal empty wire array and directly contradict §12 — the correct total-functions discipline is to make the *consumer* (`messageDoc`/`helpDoc`, Phase 4) total over the empty case, not to make the empty case unrepresentable.

### 2.6 Promotion, per version

```haskell
-- | Total per case, one clause per RawDiagnostic constructor — again
-- exhaustively checked because RawDiagnostic is a GADT indexed by a
-- closed kind.
promote :: SomeRawDiagnostic -> Either DecodeError GhcDiagnostic
promote (SomeRawDiagnostic SV1_0 (RawDiagnosticV1_0 f)) = promoteV1_0 f
promote (SomeRawDiagnostic SV1_1 (RawDiagnosticV1_1 f)) = promoteV1_1 f
promote (SomeRawDiagnostic SV1_2 (RawDiagnosticV1_2 f)) = promoteV1_2 f

promoteV1_0 :: RawFieldsV1_0 -> Either DecodeError GhcDiagnostic
promoteV1_0 f = do
  ver <- mkGhcVersion (rf10Version f)
  sev <- mkGhcSeverity (rf10Severity f)
  code <- traverse mkGhcDiagnosticCode (rf10Code f)
  span_ <- traverse promoteSpan (rf10Span f)
  pure GhcDiagnostic
    { ghcVersion  = ver
    , ghcSpan     = span_
    , ghcSeverity = sev
    , ghcCode     = code
    , ghcMessage  = rf10Message f
    , ghcHints    = rf10Hints f
    , ghcReason   = Nothing        -- schema 1.0 has no reason field
    , ghcRendered = Nothing        -- schema 1.0 has no rendered field
    }
-- promoteV1_1, promoteV1_2 follow the same shape, additionally populating
-- ghcReason (via promoteReason) and, for 1.2, ghcRendered.
```

Note the shape of `promoteV1_0`: `ghcReason`/`ghcRendered` are `Nothing` **by the schema's own absence of the field**, not by a wire-level `Maybe` collapsing "absent" and "present-but-null" into one case — schema 1.0 simply has no such field in `RawFieldsV1_0`, so there is nothing to be ambiguous about, which is exactly the benefit §8 asks for from schema-specific raw types.

**`reason`/`rendered` lifecycle, stated completely (I-29):**

- **Version availability.** `ghcReason` can only be non-`Nothing` for a value promoted from `RawFieldsV1_1` or `RawFieldsV1_2` (`rf11Reason`/`rf12Reason`), never from `RawFieldsV1_0` — there is no `rf10Reason` field for `promoteV1_0` to read, so the absence is structural, not a runtime check. `ghcRendered` can only be non-`Nothing` for a value promoted from `RawFieldsV1_2` (`rf12Rendered`), for the same structural reason; this specification treats schema "1.2" as the provisional version this field first appears under, per Phase 1.3's note, and §14's checklist marks that specific label **External verification required** — the *availability rule itself* ("absent below the version it was introduced in, structurally, not merely usually null") is Design-fixed regardless of which exact version number is eventually confirmed. **POST-FREEZE:** the label is now Verified (see Phase 1.3's update); the availability rule above still holds exactly as stated — `ghcRendered` remains `Maybe RenderedDiagnostic` at the `GhcDiagnostic` level because that `Maybe` reflects *which schema version* promoted the value (absent, structurally, below 1.2), not optionality *within* schema 1.2 itself, where the wire field is now known to be required.
- **Raw preservation.** Once promoted, `ghcReason`/`ghcRendered` are carried on `GhcDiagnostic` completely unmodified from their `RawReason`/`Text` wire values — `RenderedDiagnostic` (§2.4) wraps the `rendered` string exactly as received, with no re-formatting, re-wrapping, or whitespace normalization, and `DiagnosticReason` (§2.4) is a direct structural copy of `RawReason`'s two-shape `oneOf`, not a lossy simplification of it (e.g. `RawReasonFlags`'s `NonEmpty Text` is preserved as a `NonEmpty Text`, never flattened to a single joined string).
- **No reparsing.** `ghcRendered`'s `Text` is never fed back into `decodeDiagnosticLine`, `parseJsonValue`, or any other parser in this codebase, and never inspected for embedded structure (e.g. scanning it for a diagnostic code or file path) — it is opaque, pre-rendered display text from GHC's own perspective, consistent with §15's "rendered but never reparsed" rule and with the "no `Read` instances" style rule (§12 of this document).
- **Projection rule.** Neither field is ever mapped onto a `Tadka.Diagnostic` method. `Tadka.message`/`Tadka.help` (§5.1) are built from `ghcMessage`/`ghcHints` only; `ghcReason`/`ghcRendered` have no corresponding Tadka field to be silently reinterpreted as, and no instance method in §5.1 or §5.3 reads either one. They exist on `GhcDiagnostic` purely so a caller working directly with GHC's own semantics (not through Tadka rendering) can still reach them.

Version-specific fixtures (§9.1's `schema/1.1/optional-reason.json`, `reason-flags.json`, `reason-category.json`, and `schema/1.2/optional-rendered.json`) exercise the availability rule directly: each confirms the corresponding field is present when its schema declares it and structurally absent (not merely null) when decoded from an earlier schema version's fixture. **POST-FREEZE:** `schema/1.2/optional-rendered.json` has been renamed `schema/1.2/rendered.json` to stop implying optionality now that the field is confirmed required; a new `schema/1.2/missing-rendered.json` fixture was added specifically to exercise the now-correct failure mode (a schema-1.2 diagnostic omitting `rendered` is a decode error, not a silently-absent field).

### 2.7 Decoding mode and the primitive decoder

§25 requires the forward-compatible/strict choice to be deliberate and requires dropped fields to remain inspectable rather than silently discarded. Both requirements need a concrete API, not just a policy statement:

```haskell
-- | Which of the two decoding policies §25 describes is in effect.
-- 'Strict' is intended for conformance testing against the fixture
-- corpus (Phase 9); 'ForwardCompatible' is the production default.
data DecodingMode = Strict | ForwardCompatible
  deriving stock (Eq, Show)

-- | A field present on the wire but not recognised by the matched
-- schema version's field set — its key *and* its own raw JSON value
-- (I-25), not just its name, so the unknown data is genuinely inspectable
-- rather than merely nameable. Never silently discarded in
-- 'ForwardCompatible' mode — instead it is returned alongside the
-- successfully decoded value, so dropping it is visible and auditable.
-- Warnings for a given object are produced in the same order the
-- object's keys appeared on the wire — Aeson's underlying key-value map
-- preserves insertion order — so this ordering is deterministic and
-- reproducible across identical input, not an artifact of however a
-- particular Map/HashMap happens to be traversed.
data DecodeWarning = UnknownField Text Aeson.Value
  deriving stock (Eq, Show)

-- | The general form. Total: 'Strict' mode rejects any object key not in
-- the matched version's recognised field set as a DecodeError before
-- promotion is attempted; 'ForwardCompatible' mode promotes exactly as
-- before and separately reports every recognised-schema-version object
-- key it did not consume.
decodeDiagnosticLineWith
  :: DecodingMode -> ByteString
  -> Either DecodeError (GhcDiagnostic, [DecodeWarning])
decodeDiagnosticLineWith mode bs = do
  value <- parseJsonValue bs
  obj   <- expectObject value
  rawSv <- extractSchemaVersionField obj
  case classifySchemaVersion rawSv of
    Left (UnsupportedSchemaVersion sv) -> Left (DecodeUnsupportedVersion sv)
    Right (SomeSKnownSchemaVersion sv) -> case mode of
      Strict ->
        rejectUnknownFields sv obj *> fmap (\d -> (d, [])) (decodeKnown sv obj)
      ForwardCompatible ->
        (,) <$> decodeKnown sv obj <*> pure (unknownFieldWarnings sv obj)
  where
    decodeKnown sv obj = decodeRawByVersion sv obj >>= promote . SomeRawDiagnostic sv

-- | The single normative primitive (§22, §33), preserved with its
-- original type unchanged so every other phase's use of it is
-- unaffected: production-default forward-compatible decoding, with any
-- warnings discarded for callers who only need §22's original contract.
decodeDiagnosticLine :: ByteString -> Either DecodeError GhcDiagnostic
decodeDiagnosticLine bs = fst <$> decodeDiagnosticLineWith ForwardCompatible bs
```

`rejectUnknownFields`/`unknownFieldWarnings` are total functions over the object's key set compared against the known field-name set for the matched `SKnownSchemaVersion` (one such set per version, kept alongside each `parseFieldsVX_Y`). `decodeDiagnosticLine`'s type is exactly as it appears everywhere else in this document (Phase 7, Phase 8), so no other phase needs to change to accommodate this refinement.

**Definition of done:** invariants #1, #2, #3, #6, #7, #16 (§32) are all directly exercised by this phase's fixtures; the §31.1 fixture list (minimal/maximal diagnostic per version, missing required field, wrong field type, null code, null span, empty message array, empty hints, unknown fields) all pass against `decodeDiagnosticLine`; an additional fixture pair (`unknown-fields.json` decoded once in each `DecodingMode`) confirms `Strict` rejects it and `ForwardCompatible` accepts it while reporting a non-empty `[DecodeWarning]`.

---

## Phase 3 — Coordinate Conversion and Source/Span Model (resolves §33 "exact GhcSpan", coordinate-conversion open items, source-provider abstraction)

**Depends on:** Phase 2.
**Vision anchors:** §16 (Source and Span Model), §17 (Source Binding), §18 (Span State), §19 (Coordinate Conversion), §20 (Multi-Source Diagnostics).

### 3.1 Coordinate-conversion decisions

§19 lists these as open items. Each is resolved here explicitly. Status is marked per row: **Confirmed** rows are backed by GHC's published schema or a real captured diagnostic line (Phase 1.3); **Provisional** rows are this specification's chosen convention, still to be verified against live GHC output during Phase 9 fixture collection before implementation freeze:

| Question | Decision | Status |
|---|---|---|
| One-based or zero-based coordinates? | One-based, matching GHC's traditional `SrcSpan` convention. | Provisional |
| Column unit? | Unicode code points, not UTF-16 code units or bytes. | Provisional |
| Endpoint convention? | End line/column is **exclusive** (points one past the last covered character), so a zero-width span is naturally `start == end` with no special case. | **Confirmed** — a real captured line shows a one-character token spanning column 7 to column 8 (Phase 1.3), which only makes sense under an exclusive-end convention. |
| Newlines | `\n` is the line separator for counting purposes. | Provisional |
| CRLF | The `\r` of a `\r\n` pair is not counted as part of either line's column range. | Provisional |
| Tabs | Counted as exactly one column each; not expanded to a tab stop. | Provisional |
| Out-of-range coordinates | Rejected explicitly (`SourceBindingError`), never clamped or silently corrected. | This is a design choice made by this specification, not a fact about GHC's output, so there is nothing to confirm — it holds regardless of GHC's behaviour. |

### 3.2 Validated coordinate types

```haskell
newtype Line = Line Int
  deriving stock (Eq, Ord, Show)

newtype Column = Column Int
  deriving stock (Eq, Ord, Show)

-- | Total: rejects both ends of the range a wire Integer could fall
-- outside of. Bounded to 'Int', not 'Word': every downstream use of a
-- validated Line/Column (coordinateToOffset, Phase 3.5) is Int
-- arithmetic against a character offset that is itself an 'Int'.
-- Storing the validated value as a 'Word' would only defer the overflow
-- this smart constructor exists to prevent to a later, unchecked
-- 'fromIntegral :: Word -> Int' conversion at the point of use — exactly
-- the bug this constructor is supposed to close off (I-11). Bounding to
-- 'Int' here means no conversion between numeric domains happens again
-- anywhere downstream of this function.
mkLine :: Integer -> Either DecodeError Line
mkLine n
  | n < 1                            = Left (InvalidCoordinate "line numbers are 1-based; got " n)
  | n > toInteger (maxBound :: Int)  = Left (InvalidCoordinate "line number exceeds Int range; got " n)
  | otherwise                        = Right (Line (fromInteger n))

mkColumn :: Integer -> Either DecodeError Column
mkColumn n
  | n < 1                            = Left (InvalidCoordinate "columns are 1-based; got " n)
  | n > toInteger (maxBound :: Int)  = Left (InvalidCoordinate "column exceeds Int range; got " n)
  | otherwise                        = Right (Column (fromInteger n))
```

### 3.3 `GhcSpan`

```haskell
data GhcSpan = GhcSpan
  { spanFile      :: FilePath  -- preserved verbatim, §20
  , spanStartLine :: Line
  , spanStartCol  :: Column
  , spanEndLine   :: Line
  , spanEndCol    :: Column
  } deriving stock (Eq, Show)

mkGhcSpan :: FilePath -> Line -> Column -> Line -> Column -> Either DecodeError GhcSpan
mkGhcSpan file sl sc el ec
  | (el, ec) < (sl, sc) = Left (InvalidCoordinate "span end precedes start" 0)
  | otherwise            = Right (GhcSpan file sl sc el ec)

promoteSpan :: RawSpan -> Either DecodeError GhcSpan
promoteSpan rs = do
  sl <- mkLine   (rpLine   (rsStart rs))
  sc <- mkColumn (rpColumn (rsStart rs))
  el <- mkLine   (rpLine   (rsEnd   rs))
  ec <- mkColumn (rpColumn (rsEnd   rs))
  mkGhcSpan (Text.unpack (rsFile rs)) sl sc el ec
```

### 3.4 Explicit source provider (§17: "must not silently read arbitrary files")

```haskell
newtype SourceText = SourceText Text
  deriving stock (Eq, Show)

-- | A source-lookup attempt can fail in ways that are not "the file
-- doesn't exist" (I-06): the path may exist but be unreadable (permission
-- denied), the underlying storage may fail transiently (an IO error not
-- specific to this file), or the bytes read may not be valid text in the
-- encoding this provider assumes. Each is represented as data, not an
-- exception escaping 'lookupSource'.
data SourceLookupError
  = SourceIOError Text          -- ^ underlying IO failure, rendered as text; not tied to a specific exception type so this stays usable across providers backed by different IO mechanisms
  | SourceInvalidEncoding Text  -- ^ the file was read successfully but its bytes are not valid text in this provider's assumed encoding
  deriving stock (Eq, Show)

-- | Explicit, pure-in-spirit capability: the caller decides how source is
-- obtained (disk, in-memory cache, editor buffer, a test stub returning
-- canned text). tadka-ghc never reaches for the filesystem on its own.
-- 'Left' is a genuine lookup failure (I-06); 'Right Nothing' is "looked
-- up cleanly, no such source" (the ordinary not-found case, still not an
-- error — §2's "must remain useful even when no source text is
-- available"); 'Right (Just _)' is success.
newtype SourceProvider m = SourceProvider
  { lookupSource :: FilePath -> m (Either SourceLookupError (Maybe SourceText)) }
```

A concrete `IO`-backed instance (`fileSourceProvider :: SourceProvider IO`) is provided as an opt-in convenience in `Tadka.GHCProtocol.Span`, but it is never invoked implicitly by any decode or promotion function — only by a caller who explicitly passes it to `bindSpan`. `fileSourceProvider` is required to distinguish a missing file (`Right Nothing`) from a permission or IO failure (`Left (SourceIOError _)`) and from invalid encoding (`Left (SourceInvalidEncoding _)`), never collapsing any of the three into another by construction, catching only the specific IO exceptions that correspond to those cases and letting any other exception propagate rather than being silently absorbed.

### 3.5 Span state — exactly the states §18's table enumerates, no more

```haskell
data SpanState
  = NoSpan
  | SpanNoSource GhcSpan
  | SpanSourceUnavailable GhcSpan SourceLookupError
    -- ^ Source lookup itself failed (permission, IO, or encoding error,
    -- Phase 3.4) — distinct from 'SpanNoSource', which means the lookup
    -- succeeded and cleanly found nothing (I-06). Both still leave the
    -- diagnostic usable without location context; only the reason differs.
  | SpanInvalidCoordinates GhcSpan SourceBindingError
  | SpanBound Tadka.Context
  deriving stock (Show)
```

`SpanNoSource` is deliberately **not** an error. §2 requires a GHC diagnostic to "remain useful even when no source text is available," so a missing source file degrades gracefully to a diagnostic with no location context rather than failing — `SourceBindingError` (below) is reserved for the genuinely erroneous case: coordinates that don't fit the source that *was* found.

The fifth row of §18's table — a span becoming invalid against source that changed after the diagnostic was issued — is not implemented in this phase. §18 itself (as amended) marks that row as an editor-consumer concern outside this project's non-goals (§1), listed only so the representation doesn't foreclose it. `SpanState` therefore has no `SpanStale` constructor yet; adding one later is a additive, non-breaking change precisely because every consumer of `SpanState` is required by `-Wincomplete-patterns` to handle new constructors when they're added, which will surface every call site that needs updating.

```haskell
data SourceBindingError
  = InvalidCoordinates GhcSpan Text  -- coordinates don't fit the source found
  deriving stock (Show)

bindSpan :: Monad m => SourceProvider m -> Maybe GhcSpan -> m SpanState
bindSpan _        Nothing     = pure NoSpan
bindSpan provider (Just spn)  = do
  found <- lookupSource provider (spanFile spn)
  pure $ case found of
    Left err         -> SpanSourceUnavailable spn err
    Right Nothing    -> SpanNoSource spn
    Right (Just src) -> case convertSpan src spn of
      Left err  -> SpanInvalidCoordinates spn err
      Right ctx -> SpanBound ctx

-- | Per-line metadata: the character offset (0-based) its first
-- character starts at, and the number of columns of *content* it
-- contains — deliberately excluding a trailing '\r' where the source
-- uses CRLF line endings (I-07): the CR is a line-ending byte, not a
-- source column, and must not be reachable as one when validating an
-- end-of-line coordinate.
data LineMetadata = LineMetadata
  { lineStartOffset   :: Int
  , lineContentLength :: Int
  } deriving stock (Eq, Show)

-- | Total: '\n' is the sole line separator for counting purposes (§3.1),
-- so the source is split on it once, and each resulting segment's
-- trailing '\r', if any, is stripped before the segment's length is
-- measured — this is what actually keeps the CR in a CRLF pair out of
-- the preceding line's counted column range (I-07), rather than only
-- documenting that it should be excluded while a plain '\n'-only line
-- length would still count it. Built by one linear left-to-right pass
-- over the already-split segments, each visited exactly once (I-12) —
-- no segment is re-scanned or re-sliced from an ever-shrinking suffix,
-- so construction cost is proportional to the number of lines, not to
-- the square of the source length. A source ending in a trailing '\n'
-- correctly produces a final, empty segment of content length 0 — an
-- empty final line, not an absent one (I-10) — because 'Text.splitOn'
-- on a string ending in the separator always yields a trailing empty
-- segment.
computeLineMetadata :: Text -> NonEmpty LineMetadata
computeLineMetadata src = go 0 (Text.splitOn "\n" src)
  where
    go !offset [l]      = LineMetadata offset (contentLength l) :| []
    go !offset (l : ls) =
      let meta     = LineMetadata offset (contentLength l)
          consumed = Text.length l + 1  -- +1 for the '\n' just split on
      in meta <| go (offset + consumed) ls
    go !offset []       =
      LineMetadata offset 0 :| []  -- unreachable: splitOn never returns []
    contentLength l
      | Text.isSuffixOf "\r" l = Text.length l - 1
      | otherwise              = Text.length l

-- | Total: converts a 1-based (Line, Column) pair into a 0-based
-- character offset, using the per-line metadata above, rejecting any
-- coordinate whose line does not exist or whose column exceeds that
-- line's own content length. The endpoint convention (§3.1) is exclusive
-- — one position past the last content character is a legal column,
-- representing "end of this line's content" (I-10) — so the bound
-- checked here is 'column - 1 > lineContentLength', not '>='; an empty
-- line (content length 0) therefore accepts exactly column 1, the only
-- legal zero-width position on it. Returns a plain reason 'Text', not
-- 'SourceBindingError' directly, so this function stays reusable
-- independent of which 'GhcSpan' the caller is validating a coordinate
-- against.
coordinateToOffset :: NonEmpty LineMetadata -> Line -> Column -> Either Text Int
coordinateToOffset lineMeta (Line l) (Column c) =
  case drop (l - 1) (NE.toList lineMeta) of
    [] -> Left "line number exceeds source"
    (LineMetadata start len : _)
      | c - 1 > len -> Left "column exceeds line length"
      | otherwise   -> Right (start + c - 1)

-- | Pure, total: implements the decisions in §3.1 above in full, and
-- constructs a real Tadka context via the actual public pipeline (I-02),
-- not an assumed 'Tadka.mkContext' signature: character offsets are
-- turned into a 'Tadka.Span' via 'Tadka.mkSpan offset length', wrapped
-- into a named source via 'Tadka.mkNamedSource', attached as a single
-- primary label via 'Tadka.Labeled', and assembled into a 'Tadka.Context'
-- via 'Tadka.mkContext namedSource labels'. No label text is attached
-- beyond marking the span itself as primary — GHC's own diagnostic
-- carries no separate annotation string for this location at the point
-- this function is called, so none is fabricated (§1's "never distort
-- data" principle, applied here as much as to GHC's own JSON fields).
convertSpan :: SourceText -> GhcSpan -> Either SourceBindingError Tadka.Context
convertSpan (SourceText src) sp =
  case (startOffset, endOffset) of
    (Left reason, _) -> Left (InvalidCoordinates sp reason)
    (_, Left reason)  -> Left (InvalidCoordinates sp reason)
    (Right s, Right e)
      | e < s     -> Left (InvalidCoordinates sp "end offset precedes start offset")
      | otherwise ->
          let namedSource = Tadka.mkNamedSource (spanFile sp) src
              tadkaSpan    = Tadka.mkSpan s (e - s)
              labels       = [Tadka.Labeled tadkaSpan Tadka.Primary Text.empty]
          in Right (Tadka.mkContext namedSource labels)
  where
    lineMeta    = computeLineMetadata src
    startOffset = coordinateToOffset lineMeta (spanStartLine sp) (spanStartCol sp)
    endOffset   = coordinateToOffset lineMeta (spanEndLine sp)   (spanEndCol sp)
```

**Definition of done:** invariants #8, #9, #15 (§32) hold; the §31.2 coordinate fixture list (first character, same-line spans, multi-line spans, end-of-line, final character, empty spans, Unicode, CRLF, tabs, invalid coordinates, out-of-range coordinates) all pass against `convertSpan`; a dedicated CRLF fixture confirms a span ending immediately before a `\r\n` pair converts to the same offset it would if the source instead used a bare `\n` there (I-07); `bindSpan` is total for every combination of `Maybe GhcSpan` and every possible `SourceProvider` result, including `Left` lookup failures, verified by a Hedgehog property (Phase 9); `computeLineMetadata` and `coordinateToOffset` are exercised directly by unit tests, not only indirectly through `convertSpan`, since they are the two functions carrying the actual coordinate-conversion logic; `computeLineMetadata` is additionally benchmarked (or property-tested for linear scaling) against a large representative source to confirm the O(n²) risk this replaces (I-12) does not reappear; a real integration test constructs a `Tadka.Context` from a GHC span end-to-end through `Tadka.mkNamedSource`/`Tadka.mkSpan`/`Tadka.Labeled`/`Tadka.mkContext` and renders it (I-02's acceptance gate).

Per §3.1's status column, the one-based/code-point/CRLF/tab conventions above remain **provisional** until verified against pinned real GHC fixtures during Phase 9 (I-08); until that verification is complete and recorded, the implementation-freeze gate for this phase stays **blocked**, not merely noted, per this specification's own freeze rule (§8 of the fix register / this document's Consistency Checklist, §14).

---

## Phase 4 — Error Algebra (resolves §33 "exact DecodeError/SourceBindingError/TadkaConversionError")

**Depends on:** Phases 1–3.
**Vision anchor:** §24 (Error Algebra).

```haskell
-- | An ordered path of JSON object keys from the decoded value's root to
-- the field a structural error concerns, outermost first (e.g.
-- ["span","start","line"] for a malformed nested line number). Empty for
-- a top-level field. A plain type alias, not a newtype: it is
-- constructed and consumed only inside this module's own parsers, never
-- part of a public smart-constructor's input, so the extra indirection a
-- newtype would add has no corresponding safety benefit here.
type JsonPath = [Text]

data DecodeError
  = DecodeMalformedJson Text
  | DecodeNotAnObject
  | DecodeMissingField SchemaVersion JsonPath Text
    -- ^ schema version in effect, path to the enclosing object, missing
    -- field name (I-26) — e.g. version "1.1", path ["span","start"],
    -- field "line" identifies exactly which nested object was missing
    -- which key, not merely that some field somewhere was missing.
  | DecodeFieldTypeMismatch SchemaVersion JsonPath Text Text
    -- ^ schema version, path to the enclosing object, field name,
    -- expected type (I-26) — the same structural context as
    -- 'DecodeMissingField', so a wrong-type error at
    -- ["span","end"] / "column" is distinguishable from the same field
    -- name colliding elsewhere in the document.
  | DecodeUnsupportedVersion SchemaVersion
  | InvalidGhcVersion Text
  | InvalidDiagnosticCode Integer
  | UnrecognizedSeverity Text
  | InvalidCoordinate Text Integer
  deriving stock (Eq, Show)

-- SourceBindingError: defined in Phase 3.5. Deliberately does not include
-- a "source unavailable" constructor — that case is a SpanState value
-- (SpanNoSource for a clean not-found, SpanSourceUnavailable for a
-- genuine lookup failure, Phase 3.4/I-06), not a failure of coordinate
-- conversion, per §18's explicit "GHC diagnostic must remain useful even
-- when no source text is available" (§2).

-- | Reserved for future genuine Tadka-side semantic mapping failures.
-- Currently uninhabited: the base adapter's projection (Phase 5) is
-- provably total, so no constructor exists to be thrown. An empty type is
-- a stronger, more honest statement of "this cannot fail" than an Either
-- whose Left is never constructed in practice — Phase 5's projection
-- functions therefore have no Either in their signatures at all, and this
-- type does not appear in any function signature anywhere in this
-- specification; it exists purely as a documented extension point.
-- Declaring a data type with zero constructors requires the
-- 'EmptyDataDecls' extension, added to Phase 2's mandatory
-- default-extensions list for exactly this purpose. No 'deriving' clause
-- is written: a type with no constructors has no values to show or
-- compare, so there is nothing for a derived instance to do, and writing
-- one would only invite a reader to wonder what case it covers.
data TadkaConversionError
```

`parseFieldsV1_0`/`V1_1`/`V1_2` (Phase 1.4) build their `JsonPath` argument by threading the current key-access path down through each nested `Aeson.Object`/`Aeson..:`-style lookup they perform — a top-level lookup starts with `[]`, and descending into `"span"` then `"start"` prepends each key in traversal order, so the path a caller sees always names the exact nested object a structural error occurred in, not just the schema version and a bare field name. This keeps `DecodeError` a closed, deterministic ADT (I-26's own requirement): the added `SchemaVersion`/`JsonPath` fields are structured data with a fixed shape, not a free-form message string that could vary between two runs over the same malformed input, so two decodes of the same bad object always produce `Eq`-identical `DecodeError` values. `render` (below) is what turns this structured context into the actionable, human-readable message I-26 asks for — e.g. "schema 1.1: field \"line\" missing at span.start" — without the structured constructor itself carrying pre-rendered text.

Each error type carries a pure, total `render :: X -> Text` function (in `Diagnostic.hs`) used both for CLI/log output and for the opaque-fallback message text — never `Show`-derived text shown to end users, since `Show` output is for debugging, not for a rendered diagnostic.

**Definition of done:** invariant #4, #5 (§32: reason not conflated with severity/help; rendered preserved but never reparsed) are enforced by construction — `DiagnosticReason` and `RenderedDiagnostic` (Phase 2) have no code path that feeds them into `GhcSeverity` or back into JSON parsing.

---

## Phase 5 — Tadka Projection (resolves §33 "exact Diagnostic instance, Context construction, severity mapping, code/reason/rendered treatment, error boundaries")

**Depends on:** Phases 2–4.
**Vision anchors:** §6 (Tadka API Boundary), §27 (Tadka Projection).

### 5.1 `GhcDiagnostic`'s Tadka instance, and the bound variant that carries `context`

The real, public `Tadka.Diagnostic` class (I-01/I-09) requires `context :: e -> Tadka.Context` — not `Maybe Tadka.Context` — and a separate `url :: e -> Maybe Tadka.Url` method this specification had previously omitted entirely. "No location" is therefore represented *within* `Tadka.Context` itself, via `Tadka.NoContext`, rather than by wrapping the whole method's result in `Maybe`; every instance below is rewritten against this actual signature.

A `Tadka.Diagnostic` instance's methods only ever receive the value the instance is declared on. `GhcDiagnostic` alone has no field holding a `SpanState` — that is computed separately, by `bindSpan` (Phase 3.5), against a `SourceProvider` the caller supplies — so an instance on `GhcDiagnostic` itself has nothing to build a real `context` from. Rather than let `context` reach for a `SpanState` that isn't there, `GhcDiagnostic`'s own instance honestly always returns `Tadka.NoContext`, and a second, explicit type pairs the two together for callers who *have* run source binding and want `context` populated:

```haskell
instance Tadka.Diagnostic GhcDiagnostic where
  severity d = case ghcSeverity d of
    SevWarning -> Tadka.SevWarning
    SevError   -> Tadka.SevError
  -- Total, exactly the two-case mapping §11 specifies. No Advice branch
  -- exists to write, because GhcSeverity itself has only two constructors.

  message d = messageDoc (ghcMessage d)

  help d = helpDoc (ghcHints d)

  code _ = Nothing
  -- §10's base-adapter rule: never fabricate a Tadka DiagnosticCode from
  -- GHC's numeric one. The original is still reachable via ghcCode on the
  -- GhcDiagnostic value itself — nothing is lost, it is simply not
  -- projected into this field.

  related _         = []
  diagnosticId _     = Nothing
  diagnosticCause _  = Nothing
  -- §21, verbatim.

  url _ = Nothing
  -- I-01: the real Tadka.Diagnostic class requires this method; GHC's
  -- own JSON diagnostics carry no associated URL, so nothing is
  -- fabricated to populate it.

  context _ = Tadka.NoContext
  -- Honest, not merely convenient: no source binding has been attempted
  -- against a plain GhcDiagnostic, so there is nothing this instance
  -- could return other than the real API's own "no location" value. A
  -- caller who has bound source material uses BoundGhcDiagnostic below
  -- instead.

-- | A GhcDiagnostic together with the outcome of binding it against
-- source material (Phase 3.5). This, not GhcDiagnostic itself, is the
-- type whose Tadka instance can honestly report a location context.
data BoundGhcDiagnostic = BoundGhcDiagnostic
  { boundDiagnostic :: GhcDiagnostic
  , boundSpanState  :: SpanState
  }

instance Tadka.Diagnostic BoundGhcDiagnostic where
  -- Every delegated method is explicitly qualified with the 'Tadka.'
  -- prefix (I-03): inside this very instance declaration, an
  -- unqualified method name is a potential trap — a maintainer reading
  -- or editing this instance later could shadow it with a local
  -- binding, or a refactor could otherwise make the name resolve back
  -- into this instance's own definition rather than the class method
  -- dispatched on 'GhcDiagnostic'. Explicit qualification removes that
  -- ambiguity by construction rather than relying on it never being
  -- introduced.
  severity        = Tadka.severity . boundDiagnostic
  message         = Tadka.message . boundDiagnostic
  help            = Tadka.help . boundDiagnostic
  code            = Tadka.code . boundDiagnostic
  related         = Tadka.related . boundDiagnostic
  diagnosticId    = Tadka.diagnosticId . boundDiagnostic
  diagnosticCause = Tadka.diagnosticCause . boundDiagnostic
  url             = Tadka.url . boundDiagnostic

  context b = case boundSpanState b of
    SpanBound ctx              -> ctx
    NoSpan                     -> Tadka.NoContext
    SpanNoSource _             -> Tadka.NoContext
    SpanSourceUnavailable _ _  -> Tadka.NoContext
    SpanInvalidCoordinates _ _ -> Tadka.NoContext
  -- Total, one equation per SpanState constructor (compiler-checked
  -- exhaustiveness — SpanState gaining another constructor later, e.g.
  -- a future SpanStale per Phase 3.5's note, is a compile error here
  -- until this case is updated, not a silent Tadka.NoContext by
  -- omission).
```

Every other method on `BoundGhcDiagnostic` simply delegates to the inner `GhcDiagnostic`'s own instance via qualified function composition, so there is exactly one place (`context`) where the two instances actually differ — which is also, correctly, the only place a `SpanState` is relevant at all.

### 5.2 `messageDoc` and `helpDoc` — the exact transformations §12/§13 defer to this document

**Fragment semantics, stated explicitly, since the right transformation depends on it:** GHC's `message` array (Phase 1.3) is treated as an ordered sequence of complete, independent lines of the rendered message — not word-wrapped prose fragments meant to be joined into running text, and not a set whose order carries no meaning. This reading is what GHC's own captured example shows (Phase 1.3's `["Variable not in scope: a"]`, a single complete sentence as one array element) and is consistent with `hints` being modelled identically for the same reason. Given that reading, `vsep` (vertical concatenation, one fragment per line, no wrapping or joining) is the only choice that doesn't fabricate structure: joining fragments with a space or comma would assert an adjacency relationship the array's own boundaries don't claim, and concatenating without any separator would run two independent lines together. This is why `vsep`, not `hsep`/`fillSep`/`Text.intercalate`, is used — the choice follows from the stated semantics rather than being a stylistic default.

```haskell
-- | Total over the empty-list case, which §12 explicitly requires the
-- decoder to accept without complaint.
messageDoc :: [Text] -> Doc Ann
messageDoc []    = mempty
messageDoc frags = Prettyprinter.vsep (map Prettyprinter.pretty frags)
-- Order preserved exactly (§12's invariant); no separator is invented
-- beyond the layout engine's own line breaks, and no fragment is
-- reordered, merged, or annotated with meaning that wasn't in the input.

-- | Nothing when there are no hints, distinguishing "no hints" from
-- "hints present" at the type level rather than rendering an empty
-- bulleted list (§13: "avoid silently discarding hints" — here nothing
-- is discarded, there is simply nothing to render).
helpDoc :: [Text] -> Maybe (Doc Ann)
helpDoc []    = Nothing
helpDoc hints = Just (Prettyprinter.vsep (map (("• " <>) . Prettyprinter.pretty) hints))
-- Order preserved exactly (§13's invariant); same fragment semantics and
-- vsep justification as messageDoc above.
```

This is registered as a tested invariant, not left as a design note only: `prop_messageDoc_preserves_fragment_count` (Phase 9.3) asserts that for any non-empty `frags`, splitting the `Text` `messageDoc frags` renders to (via `Prettyprinter.Render.Text.renderStrict` at a wide layout, §5.2's note below) on `\n` yields exactly `length frags` lines — i.e. that fragments are genuinely kept separate, not merely that their text survives in some order.

**Empty message array stays a valid, distinguishable diagnostic (I-27).** `messageDoc [] = mempty` means the rendered `message` is empty, but that is the only field this specification lets go empty this way — `severity`, `context` (or `Tadka.NoContext`), and, where present, `ghcCode`/`ghcReason` are all still populated exactly as for any other diagnostic, and none of §5.1's other instance methods change behaviour based on whether `ghcMessage` happens to be `[]`. A GHC diagnostic with an empty message array is therefore never confused with "no diagnostic at all": Tadka's own renderer composes `message` alongside these other fields rather than gating the whole diagnostic's visibility on `message` being non-empty, so an empty-message diagnostic still surfaces as a genuine, located (or explicitly unlocated) diagnostic of the reported severity — merely one with nothing further to say beyond that. This is exercised end-to-end, not asserted only at the `messageDoc` level: `test/fixtures/schema/{1.0,1.1,1.2}/empty-message.json` (already listed in §9.1) is carried one step further than decoding alone — a golden test renders each decoded value through the real `Tadka.Diagnostic GhcDiagnostic` instance and confirms the rendered output is a well-formed, non-empty diagnostic block (severity and location/no-location are both visibly present) with only the message region itself blank, rather than an empty or missing block.

### 5.3 `OpaqueGhcOutput`'s Tadka instance — thin, by design

```haskell
instance Tadka.Diagnostic OpaqueGhcOutput where
  severity _ = Tadka.SevError
  message o  = Prettyprinter.pretty (opaqueText o)
  help _         = Nothing
  code _         = Nothing
  related _      = []
  diagnosticId _ = Nothing
  diagnosticCause _ = Nothing
  url _          = Nothing
  context _      = Tadka.NoContext
  -- No span, no code, no hints, no reason: none of that genuinely existed
  -- in captured output, so none of it is fabricated (§4 of the vision,
  -- and the "never distort data" principle from §1, applied here to
  -- unstructured data exactly as it is applied to GHC's JSON).
```

**Definition of done:** invariants #3, #10, #12, #14 (§32) hold; §31.3's Tadka-projection test list (Warning→SevWarning, Error→SevError, message preservation, hint ordering, source/file preservation, no fabricated Tadka code, no fabricated related/cause/id, rendered output remains inert metadata) all pass; `code`, `related`, `diagnosticId`, `diagnosticCause` for `GhcDiagnostic` are each one-line total functions with no `Either`/`Maybe`-unwrapping partiality; an additional test confirms `GhcDiagnostic`'s own `context` is always `Tadka.NoContext`, and that `BoundGhcDiagnostic`'s `context` matches `SpanBound`'s payload exactly for all five `SpanState` constructors, so the split introduced in §5.1 is itself exercised, not merely declared; `tadka-ghc` compiles against the pinned `tadka` release with `-Wall -Wextra -Wincomplete-patterns -Werror` (I-01's acceptance gate), with no instance in this module left with an unimplemented `url` or a `Maybe`-wrapped `context`.

---

## Phase 6 — Build Tool Invocation (resolves §33 "cabal vs stack detection, flag-injection mechanism, precedence rules, subprocess capture, exit code handling, timeout/cancellation")

**Depends on:** Phase 0 only (independent of Phases 1–5; may be developed in parallel).
**Vision anchor:** §3 (Build Tool Invocation).
**Component note (I-35):** §6.1's types and §6.2/§6.3's pure detection and flag-injection functions belong to the core `tadka-ghc` library (`BuildTool.hs`/`Process.hs`, per §2's module layout); §6.4's `runBuild` — the one function in this phase that performs process I/O — belongs to the separate `tadka-ghc-process` component (`Runner.hs`), consumed by the `tadka-ghc` executable rather than by the core library itself. Nothing in §6.1–§6.3 or §6.5 below depends on `Runner.hs`, so this split is a packaging change, not a semantic one — every type and function signature in this phase is exactly as it would be undivided.

### 6.1 Types

```haskell
data BuildTool = Cabal | Stack
  deriving stock (Eq, Show)

data BuildToolDetectionError
  = NoRecognizedProjectFile
  | AmbiguousProjectFiles (NonEmpty FilePath)
  deriving stock (Eq, Show)

data OutputStream = StdOut | StdErr
  deriving stock (Eq, Show)

-- | The vision's own richer process-outcome model (§3, I-23), carried
-- into the implementation rather than reduced to a bare 'ExitCode': a
-- timeout and a signalled termination are both genuinely distinct from
-- an ordinary non-zero exit and from each other (I-18), and a process
-- that never started at all (bad executable path, permission denied) is
-- distinct again from one that started and then failed.
data CompilerResult
  = CompilerSucceeded
  | CompilerFailed ExitCode
  | CompilerSignalled Signal
  | CompilerTimedOut
  | CompilerStartFailed ProcessError
  deriving stock (Show)

-- | The POSIX/OS signal number that terminated the child process, where
-- the runtime is able to surface one. Kept as a bare, documented newtype
-- rather than importing a specific process-execution library's own
-- signal type, since this specification does not commit to one such
-- library's representation over another.
newtype Signal = Signal Int
  deriving stock (Eq, Show)

-- | Wraps the underlying IO exception raised when the child process
-- itself could not be started (executable not found, permission denied,
-- working directory missing, etc.) — distinct from every
-- 'CompilerResult' constructor above, all of which presuppose the
-- process actually started.
newtype ProcessError = ProcessError Text
  deriving stock (Show)

data BuildResult = BuildResult
  { compilerResult :: CompilerResult
    -- ^ A process-level fact, not a per-line one (I-16): kept as its own
    -- field, never inferred from whether the captured lines happened to
    -- decode successfully (I-17), and never collapsed into an ordinary
    -- 'ExitFailure' when the real cause was a timeout or a signal
    -- (I-18).
  , observedOutput :: [(OutputStream, ByteString)]
    -- ^ In the order this process's collector observed the two pipes
    -- deliver data, not a guarantee of the child process's true
    -- cross-stream write order (I-19): stdout and stderr are two
    -- independent OS pipes, and no collector reading them from outside
    -- the child can promise the interleaving it sees matches the exact
    -- order the child wrote to each — only that everything written to
    -- each individual stream is captured in that stream's own true
    -- order. This is documented here as the formal guarantee and must
    -- never be described elsewhere in this specification, or in its
    -- Haddock, as "production order."
  } deriving stock (Show)
```

### 6.2 Detection

```haskell
-- | The only genuinely-IO-performing function before Phase 6.4's runBuild:
-- inspects the project directory, does not compile or run anything.
-- 'Just' bypasses detection (and the filesystem check) entirely and
-- forces the given tool (I-21's "explicit override"); 'Nothing' performs
-- the detection rule below.
detectBuildTool :: Maybe BuildTool -> FilePath -> IO (Either BuildToolDetectionError BuildTool)
```

Detection is scoped and defined precisely, resolving the edge cases I-21 flags as previously unspecified:

- **Search root:** exactly the `FilePath` argument — the project directory the caller names — never an ancestor directory or a recursive walk of it.
- **Search depth:** the immediate contents of that one directory only (`readDirectory`'s single level); nested projects in subdirectories are not discovered automatically and are out of scope for this function, consistent with "GHC-specific/build-specific semantics remain in `tadka-ghc`" rather than this function reimplementing a general project-file scanner.
- **Recognised files:** exactly `stack.yaml` (any casing variant is *not* recognised — GHC toolchains on case-sensitive filesystems do not treat them as equivalent, and neither does this function), `cabal.project`, and any file matching `*.cabal`.
- **Precedence:** presence of `stack.yaml` selects `Stack`, regardless of any `.cabal`/`cabal.project` files also present — Stack projects routinely also carry a generated `.cabal` file, and treating that as ambiguous would misclassify the overwhelmingly common case. Presence of `*.cabal` or `cabal.project` **and no** `stack.yaml` selects `Cabal`. More than one `*.cabal` file (and no `stack.yaml`) is `AmbiguousProjectFiles`, since there is no principled way to prefer one over another without caller input.
- **Explicit override:** a caller who already knows which tool to use passes `Just tool` and this function performs no filesystem inspection at all, so a project with an atypical or absent layout is never blocked on detection.
- **Multiple projects:** this function only ever inspects the one directory it is given; if a caller manages multiple projects, selecting which directory to pass is the caller's responsibility, not this function's.

### 6.3 Flag injection — sound flag parsing and an explicit precedence rule

A naive text search for the flag (e.g. checking whether `"-fdiagnostics-as-json"` occurs as a substring anywhere in the argument list) is unsound: `-fno-diagnostics-as-json` contains that exact substring, so a substring check would wrongly treat an explicit *disable* as though it were the flag already being enabled. Flag detection instead recognises actual GHC option tokens, and precedence between repeated occurrences is stated explicitly rather than left implicit:

```haskell
-- | The two GHC flag tokens relevant to this decision. No other token is
-- distinguished — everything else in a --ghc-options value is opaque to
-- this function and passed through untouched.
data DiagnosticsJsonFlag = EnableDiagnosticsJson | DisableDiagnosticsJson
  deriving stock (Eq, Show)

classifyGhcFlag :: Text -> Maybe DiagnosticsJsonFlag
classifyGhcFlag "-fdiagnostics-as-json"    = Just EnableDiagnosticsJson
classifyGhcFlag "-fno-diagnostics-as-json" = Just DisableDiagnosticsJson
classifyGhcFlag _                          = Nothing

-- | Total: a single --ghc-options=<value> argument may itself contain
-- several whitespace-separated GHC flags, and cabal/stack both accumulate
-- (rather than override) repeated --ghc-options occurrences, so every
-- occurrence across the whole argument list is expanded and concatenated,
-- left to right, before any flag is classified.
extractGhcOptionTokens :: [Text] -> [Text]
extractGhcOptionTokens = concatMap tokensOf
  where
    tokensOf arg = case Text.stripPrefix "--ghc-options=" arg of
      Just value -> Text.words value
      Nothing    -> []

-- | Total (uses foldl', not the banned partial 'last', to find "the most
-- recent occurrence" of a possibly-empty sequence): GHC applies repeated
-- boolean -f/-fno flags in the order given on the command line, so the
-- last occurrence of either form is what determines the effective
-- setting; no occurrence of either form at all means it is not enabled.
diagnosticsJsonCurrentlyEnabled :: [Text] -> Bool
diagnosticsJsonCurrentlyEnabled userArgs =
  foldl' step False (mapMaybe classifyGhcFlag (extractGhcOptionTokens userArgs))
  where
    step _   EnableDiagnosticsJson  = True
    step _   DisableDiagnosticsJson = False

-- | Pure, total, idempotent. Precedence rule, stated explicitly: this
-- function never deletes or edits a user-supplied argument (satisfying
-- §32 invariant #18's "preserved, not overridden"); it only ever appends
-- its own occurrence after everything the user supplied. Because GHC's
-- last-occurrence-wins rule (above) then applies, appending after the
-- user's own arguments guarantees the flag is effectively enabled for
-- tadka-ghc's purposes even if the user's own configuration disabled it
-- with an explicit -fno-diagnostics-as-json — without textually removing
-- anything the user wrote. If the user's own last occurrence already
-- enables it, nothing further is appended, purely to avoid a redundant
-- duplicate flag (appending unconditionally would still be correct,
-- just noisier).
injectDiagnosticsFlag :: BuildTool -> [Text] -> [Text]
injectDiagnosticsFlag _tool userArgs
  | diagnosticsJsonCurrentlyEnabled userArgs = userArgs
  | otherwise = userArgs <> ["--ghc-options=-fdiagnostics-as-json"]
```

Both `Cabal` and `Stack` accept `--ghc-options=<flag>` identically for this purpose, so `_tool` is currently unused in the body — its presence in the signature is deliberate, not incidental: it documents that flag injection is a per-tool decision point, and keeps the function ready for a tool whose forwarding syntax later diverges, without changing the type signature every caller depends on.

### 6.4 Subprocess execution — the one real IO boundary

`runBuild` (living in `Runner.hs`, the `tadka-ghc-process` component — I-35) is the sole function in this whole package that performs process I/O; everything downstream of it (framing, §6.5; classification and decoding, Phase 7) is pure, and everything upstream of it in this phase (detection, §6.2; flag injection, §6.3) is pure and lives in the core library instead. Its lifecycle is specified explicitly, step by step, because "concurrent pipe draining, timeout, process termination and cleanup" cannot be implemented safely from a bare type signature (I-05) — a signature alone leaves exactly the failure modes (a lost pipe, a deadlock between the two streams, a leaked child process on timeout) this section exists to rule out:

```haskell
runBuild :: Maybe NominalDiffTime -> BuildTool -> FilePath -> [Text] -> IO BuildResult
```

Lifecycle, in order:

1. **Spawn.** Construct the child process (the build tool plus the injected arguments, §6.3) with both stdout and stderr configured as pipes the parent reads — never inherited from the parent's own handles, so nothing the child prints can leak past this function uncaptured. A failure at this step (executable not found, permission denied, working directory missing) is caught and reported as `CompilerStartFailed`; nothing in steps 2 onward runs, and no partially-spawned process is left behind.
2. **Concurrently drain both pipes.** Two reader threads are started, one per `OutputStream`, each in a loop: read one chunk from its own pipe, feed it to that stream's own `FramerState` (§6.5), and append every complete line the framer yields — tagged with its `OutputStream` — to a single shared, thread-safe output buffer, preserving the order each thread observed its own chunks arriving in. The two reader threads never share a `FramerState` (§6.5's per-stream independence) and never block on each other: a slow or silent stderr must not stall stdout draining, or vice versa, which is precisely why each pipe gets its own thread rather than one thread alternating between both — a single-threaded "read whichever pipe has data" scheme is exactly the design that can deadlock if the child fills one pipe's OS buffer while nothing is reading it.
3. **Monitor timeout, concurrently with step 2.** If a timeout was supplied, a third concurrent action races "wait for the child to exit" against "the timeout duration has elapsed," whichever happens first.
4. **On timeout:** terminate the child process, then **continue draining both pipes** until each independently reports EOF — a terminated process may still have buffered output sitting unread in its pipes, and discarding it would silently lose output the process genuinely produced before termination (this specification's standing "opaque output is never silently discarded" rule, §7 of the fix register). Each stream's `FramerState` is then flushed (`flushFramer`, §6.5) to recover its final unterminated line, if any. The result's `compilerResult` is `CompilerTimedOut` — never a synthetic `ExitFailure` (I-18) — so a timeout remains distinguishable from every ordinary exit path by construction, not by convention.
5. **On normal completion** (no timeout supplied, or the process exits before any supplied timeout elapses): draining continues until **both** reader threads have independently observed EOF on their own pipe — not merely until the process has exited, since a process can exit while its pipes still hold buffered, unread data, and that data must still be drained rather than raced against process cleanup. Each stream's `FramerState` is flushed via `flushFramer`, and the process's real termination status is awaited and recorded: a clean `ExitSuccess` becomes `CompilerSucceeded`; `ExitFailure n` becomes `CompilerFailed (ExitFailure n)`; termination by an uncaught OS signal, where the runtime is able to surface one, becomes `CompilerSignalled (Signal s)` rather than being folded into `CompilerFailed`.
6. **Cleanup under exceptions.** If any step above throws an exception that is not itself one of the outcomes above (the calling thread is killed, an unexpected IO exception escapes a reader thread), the child process is always terminated and both pipe handles are always closed before the exception is rethrown — via an exception-safe acquire/release pattern (e.g. `bracket`) wrapping the whole lifecycle, not an ad hoc cleanup call reachable only on the expected paths. No file descriptor and no child process is ever leaked, whether `runBuild` returns normally, times out, or is itself the target of an asynchronous exception.
7. **Construct the result.** `BuildResult` is built from the shared output buffer, in the order accumulated, and the `CompilerResult` determined above. `runBuild` itself never classifies the lines it captures as diagnostic-or-not — that is Phase 7's job, kept separate so the process boundary and the classification logic can each be tested independently.

This lifecycle is what Phase 9's stub-runner tests (via the `BuildRunner` capability, §9.4) exercise without a real toolchain: a fake reader can simulate a slow stderr, a hung process past its timeout, and a start failure, each asserting the corresponding `CompilerResult` and that no output already produced is lost.

### 6.5 Chunk-to-line framing

An OS pipe delivers arbitrary-sized byte chunks, not lines — a single `read` on the pipe may return half a line, several lines, or a line split across two reads. `runBuild` cannot hand `BuildResult` complete lines without an explicit, total framing step in between:

```haskell
-- | Bytes seen since the last '\n' for one stream, not yet a complete
-- line. stdout and stderr are framed with two independent FramerState
-- values — the chunk boundaries on one pipe have nothing to do with the
-- other's.
newtype FramerState = FramerState ByteString

emptyFramerState :: FramerState
emptyFramerState = FramerState BS.empty

-- | Total: feeds one raw, non-newline-aligned chunk into the framer,
-- returning every complete line it now contains (excluding the
-- trailing '\n'; CRLF is handled uniformly downstream by Phase 8's
-- stripTrailingCR, not here) and the updated pending state.
feedChunk :: FramerState -> ByteString -> (FramerState, [ByteString])
feedChunk (FramerState pending) chunk =
  let (complete, rest) = splitCompleteLines (pending <> chunk)
  in (FramerState rest, complete)

-- | Total, structural recursion on the '\n' positions found in the
-- buffer: splits it into every complete line plus whatever partial line
-- remains after the last newline (empty if the buffer ended exactly on
-- a newline). Terminates because each recursive call strictly shortens
-- its input.
splitCompleteLines :: ByteString -> ([ByteString], ByteString)
splitCompleteLines bs = case BS.elemIndex 10 {- '\n' -} bs of
  Nothing -> ([], bs)
  Just i  ->
    let (line, rest)         = BS.splitAt i bs
        (more, remainder)    = splitCompleteLines (BS.drop 1 rest)
    in (line : more, remainder)

-- | Total: called once a pipe reaches EOF, to flush a final unterminated
-- line rather than silently dropping it.
flushFramer :: FramerState -> [ByteString]
flushFramer (FramerState pending)
  | BS.null pending = []
  | otherwise       = [pending]
```

**Chosen policy (I-20): (a), unbounded buffering, accepted and documented rather than silently bounded.** No maximum line length is enforced. A pathological single line of unbounded size is accumulated in memory in full; this is an accepted, documented resource-risk limitation rather than a silent truncation, because truncating output GHC actually produced would itself be exactly the kind of data loss this specification refuses to accept everywhere else. A future revision may instead adopt policy (b) — a maximum with a controlled, explicit overflow result — but doing so is an amendment to this section, not something either policy may be silently mixed with.

**Definition of done:** invariants #17, #18 (§32) hold, verified by (a) a property test asserting `diagnosticsJsonCurrentlyEnabled (injectDiagnosticsFlag t args) == True` for arbitrary `args` (including `args` that already contain an explicit `-fno-diagnostics-as-json`), (b) a property test asserting injection is idempotent (`injectDiagnosticsFlag t (injectDiagnosticsFlag t args) == injectDiagnosticsFlag t args`), and (c) a unit test confirming `classifyGhcFlag "-fno-diagnostics-as-json" == Just DisableDiagnosticsJson` specifically, so the substring-confusion bug this section fixes cannot silently regress; `detectBuildTool` is exercised against fixture directories for all three cases (cabal-only, stack-only, ambiguous); `feedChunk`/`flushFramer` are exercised by a property test asserting that feeding an input `ByteString` split at every possible chunk boundary always reconstructs the same set of lines as splitting it in one piece.

---

## Phase 7 — Opaque Output Capture and Line Classification (resolves §33 "exact OpaqueGhcOutput, line/block grouping heuristic, minimal Diagnostic instance, encoding handling")

**Depends on:** Phase 2 (`decodeDiagnosticLine`), Phase 6 (`BuildResult`).
**Vision anchor:** §4 (Unstructured / Opaque Output Handling).

### 7.1 `OpaqueGhcOutput` — refined from the vision's sketch to be honestly lossless

```haskell
data OpaqueGhcOutput = OpaqueGhcOutput
  { opaqueSource   :: OutputStream
  , opaqueRawBytes :: ByteString
    -- ^ The captured line's (or, for a panic block, every accumulated
    -- line's) content bytes exactly as captured — never decoded, never
    -- modified. This is **payload-only**, not a byte-for-byte capture of
    -- the raw stream including delimiters (I-15): the '\n' that
    -- terminated each line on the wire is consumed by §6.5's framer and
    -- is not present here, since it is framing metadata, not diagnostic
    -- content. Any other byte the child actually wrote — including a
    -- CRLF's '\r', if the child emitted one — is preserved untouched.
    -- Exact reconstruction of the original captured stream is therefore
    -- a property of the *framed-record layer* (§6.5), not of this field
    -- in isolation: joining every record's 'opaqueRawBytes' (and every
    -- decoded diagnostic's own re-serialised line, where applicable)
    -- with '\n' in the observed order (I-19) reproduces the original
    -- stream, because '\n' is the only byte the framer ever removes. For
    -- a panic block specifically, the lines making up the block are
    -- themselves rejoined with '\n' before being stored here (§7.3), so
    -- the block's own internal delimiters are retained even though the
    -- delimiter that originally terminated the block itself is not.
  , opaqueText     :: Text
    -- ^ A total, lenient rendering of 'opaqueRawBytes' for display
    -- purposes (see §7.3's 'decodeUtf8Lenient'): it substitutes U+FFFD
    -- for any byte sequence that is not valid UTF-8, and is therefore
    -- not guaranteed to be a byte-for-byte-faithful view of
    -- 'opaqueRawBytes'. A single 'Text' field cannot honestly be both
    -- "raw" and "a valid decoding of arbitrary bytes" at once — splitting
    -- the two apart is what makes this type able to promise losslessness
    -- without also silently claiming a decoding property no total
    -- function over arbitrary bytes can have.
  , opaqueCompilerResult :: Maybe CompilerResult
    -- ^ An explicit projection of the whole build's outcome (Phase 6.1)
    -- onto this record, attached as a separate post-processing pass
    -- (§7.4) — never the primary representation of compiler
    -- success/failure (I-16): that primary representation is
    -- 'BuildOutcome.buildCompilerResult' (§7.4), which exists
    -- independently of whether any record at all was successfully
    -- classified.
  } deriving stock (Show)
```

This is a deliberate, narrow refinement of the vision document's §4 type sketch (which used one `opaqueText :: Text` field, commented "raw, unmodified") rather than a contradiction of it: the vision's own intent — nothing captured is silently lost — is what this two-field split actually delivers, since a single `Text` field populated via any total decode of arbitrary bytes cannot itself be that guarantee. §14's checklist records this explicitly as a refinement, not a silent deviation.

### 7.2 Classification result

```haskell
data LineClassification
  = ClassifiedDiagnostic GhcDiagnostic
  | ClassifiedOpaque OpaqueGhcOutput
  deriving stock (Show)
```

### 7.3 Panic-block grouping — an explicit, total finite state machine

A GHC panic prints multiple lines that belong together as one logical failure, not one `OpaqueGhcOutput` per line. This is modelled as a two-state automaton per output stream, so grouping is a provably-terminating fold rather than an ad hoc heuristic:

```haskell
data ClassifierState
  = Idle
  | AccumulatingPanic (NonEmpty ByteString)
  deriving stock (Eq, Show)

-- | Recognizes the start of a GHC panic banner. The *design* below is
-- complete (I-14): match is by prefix, not exact-string equality, so
-- trailing content on the banner's own first line (GHC appends a GHC
-- version and a short summary after "ghc: panic!" on the same line,
-- e.g. "ghc: panic! (the 'impossible' happened)\n  GHC version 9.10.1:")
-- is deliberately not required to match — only the fixed leading marker
-- is. Leading whitespace is not stripped before matching: GHC's own
-- panic banner is not observed to be indented, and stripping unobserved
-- leading whitespace would risk matching an indented line inside a
-- panic block's own body that merely happens to contain the same text.
-- A GHC-family banner using different capitalisation or punctuation
-- around "panic" (for instance, GHCi's own interactive-session panics,
-- if their wording differs) is treated as the *same* grouping case as
-- the standalone-GHC banner — this classifier does not distinguish
-- panic sub-forms into different 'LineClassification' shapes, since
-- doing so would require classifying by content this specification has
-- not committed to, not merely by the presence of a panic. What remains
-- open, and is exactly what I-14 continues to flag, is empirical, not
-- structural: whether every supported GHC family in the compatibility
-- matrix (§15) actually emits this literal prefix unmodified. That
-- match must be confirmed against real captured `ghc: panic!` output
-- per GHC family during Phase 9 fixture collection (`build/panic-*`
-- fixtures, §9.1, one per matrix cell — §15); until confirmed, this
-- function's literal remains an **external verification required**
-- item (§14), not a design gap.
isPanicMarker :: ByteString -> Bool
isPanicMarker = ("ghc: panic!" `BS.isPrefixOf`)

isBlank :: ByteString -> Bool
isBlank = BS.null . BS.dropWhileEnd isSpaceByte . BS.dropWhile isSpaceByte

-- | Total: every (state, input line) pair is handled by exactly one of
-- the four equations below, mirroring ClassifierState's two constructors
-- crossed with the two conditions checked in each. The blank line that
-- terminates a panic block is folded into the block itself (not
-- discarded and not classified separately), so every input line, without
-- exception, ends up inside exactly one LineClassification.
step
  :: OutputStream -> ClassifierState -> ByteString
  -> (ClassifierState, [LineClassification])
step stream Idle line
  | isPanicMarker line =
      (AccumulatingPanic (line :| []), [])
  | otherwise =
      case decodeDiagnosticLine line of
        Right diag -> (Idle, [ClassifiedDiagnostic diag])
        Left _err  -> (Idle, [ClassifiedOpaque (mkOpaqueLine stream line)])
step stream (AccumulatingPanic acc) line
  | isBlank line =
      (Idle, [ClassifiedOpaque (mkOpaqueBlock stream (acc <> pure line))])
  | otherwise =
      (AccumulatingPanic (acc <> pure line), [])

mkOpaqueLine :: OutputStream -> ByteString -> OpaqueGhcOutput
mkOpaqueLine stream line =
  OpaqueGhcOutput stream line (decodeUtf8Lenient line) Nothing

mkOpaqueBlock :: OutputStream -> NonEmpty ByteString -> OpaqueGhcOutput
mkOpaqueBlock stream ls =
  let raw = BS.intercalate "\n" (NE.toList ls)
  in OpaqueGhcOutput stream raw (decodeUtf8Lenient raw) Nothing

-- | Total: handles the case where a panic block runs to end-of-input with
-- no trailing blank line, so nothing accumulated is ever lost.
finalize :: OutputStream -> ClassifierState -> [LineClassification]
finalize _      Idle                    = []
finalize stream (AccumulatingPanic acc) = [ClassifiedOpaque (mkOpaqueBlock stream acc)]

-- | Total by structural recursion on a finite list; terminates because
-- each recursive call consumes exactly one list element. Classifies one
-- already-separated stream in isolation; used directly by Phase 9's
-- reconstruction property, and as the per-stream building block
-- 'classifyInterleaved' below is defined in terms of.
classifyStream :: OutputStream -> [ByteString] -> [LineClassification]
classifyStream stream = go Idle
  where
    go st []       = finalize stream st
    go st (l : ls) = let (st', out) = step stream st l in out <> go st' ls
```

`decodeUtf8Lenient` (not a partial UTF-8 decode) is used for `opaqueText` deliberately: captured process output is not guaranteed to be valid UTF-8, and a lenient decode with replacement characters is total, whereas a strict decode that can throw on malformed bytes is exactly the kind of partiality banned by §0.1. `opaqueRawBytes` is untouched by this concern entirely, since it is never decoded — this directly resolves §33's "encoding/non-UTF-8 handling for raw captured text" without trading it off against losslessness.

**Two-stream interleaving (I-04, I-13).** `classifyStream` above only handles one already-separated stream; `runBuild`'s actual output is a single interleaved `[(OutputStream, ByteString)]` list, and the two streams must be classified against **independent** `ClassifierState` values — a panic pending on stderr must not be disturbed by an unrelated stdout line arriving in between, and vice versa — while the overall output list still preserves the observed order across both streams (I-19):

```haskell
-- | One ClassifierState per OutputStream, threaded independently through
-- the single interleaved list runBuild produced.
data InterleavedState = InterleavedState
  { stdOutState :: ClassifierState
  , stdErrState :: ClassifierState
  }

initialInterleavedState :: InterleavedState
initialInterleavedState = InterleavedState Idle Idle

-- | Total by structural recursion on a finite list; terminates because
-- each recursive call consumes exactly one list element. Each line is
-- routed to its own stream's ClassifierState via 'step'; the other
-- stream's state is left completely untouched for that step, so a panic
-- accumulating on one stream is never interrupted or flushed by activity
-- on the other. A completed record — a decoded diagnostic, an ordinary
-- opaque line, or a panic block whose terminating blank line was just
-- observed — is emitted immediately, at the position in the output list
-- corresponding to where its completing input line sat in the input
-- list. This is what gives the overall result its observed-order
-- guarantee (I-19): a record completed from a stdout line and a record
-- completed from a stderr line are ordered relative to each other
-- exactly as their completing lines were ordered in the input, even
-- though each stream's own internal accumulation is otherwise
-- independent — this is a claim about *this collector's observed order*,
-- never about the child process's true cross-stream write order. At
-- end-of-input, both streams' pending states are flushed deterministically,
-- in a fixed order (stdout, then stderr), via 'finalize', so a panic left
-- pending on either stream at EOF is never silently dropped.
classifyInterleaved :: [(OutputStream, ByteString)] -> [LineClassification]
classifyInterleaved = go initialInterleavedState
  where
    go st [] =
      finalize StdOut (stdOutState st) <> finalize StdErr (stdErrState st)
    go st ((StdOut, line) : rest) =
      let (st', out) = step StdOut (stdOutState st) line
      in out <> go st { stdOutState = st' } rest
    go st ((StdErr, line) : rest) =
      let (st', out) = step StdErr (stdErrState st) line
      in out <> go st { stdErrState = st' } rest
```

### 7.4 Whole-build classification and compiler-result attachment

The compiler's result is a build-level fact, not a per-line one (I-16) — it is only known once the whole process has finished, after every line has already been classified. Rather than threading it through per-line classifier state (which would force every `OpaqueGhcOutput` but one to carry `Nothing` for no principled reason), it is attached in one explicit, total post-processing pass over the classification result, and is additionally exposed independently at the top level so it can never disappear behind a successfully-decoded record (I-17):

```haskell
-- | The top-level, whole-build result. 'buildCompilerResult' is the
-- primary, always-present fact about how the build concluded — never
-- inferred from whether any individual line happened to decode
-- successfully (I-17): a build that failed with exit code 1 but whose
-- output happened to contain nothing but perfectly valid diagnostics
-- still reports 'CompilerFailed (ExitFailure 1)' here, independently of
-- 'buildClassifications'. 'attachCompilerResult' below additionally
-- projects the same 'CompilerResult' onto every opaque record, purely as
-- a convenience for a consumer that only has one 'LineClassification' in
-- hand and no access to the enclosing 'BuildOutcome'.
data BuildOutcome = BuildOutcome
  { buildCompilerResult  :: CompilerResult
  , buildClassifications :: [LineClassification]
  } deriving stock (Show)

classifyBuildOutput :: BuildResult -> BuildOutcome
classifyBuildOutput (BuildResult result ls) =
  BuildOutcome result (attachCompilerResult result (classifyInterleaved ls))

-- | Total: attaches the build's final CompilerResult to every
-- OpaqueGhcOutput already produced — a uniform post-processing map, not
-- a special case confined to one synthetic record — so
-- 'opaqueCompilerResult' means the same thing on every record that
-- carries it: "the build this record came from concluded with this
-- result." GhcDiagnostic records are left untouched, since a decoded
-- diagnostic is a complete, valid record independent of how the build as
-- a whole concluded. If classification produced no records at all yet
-- the build did not succeed, exactly one empty synthetic record is
-- introduced — the only case a record is manufactured rather than
-- derived from captured output — so the failure is never silently lost
-- (§31.5's "non-zero exit, zero diagnostics" requirement) purely at the
-- 'LineClassification' level; 'buildCompilerResult' above makes the same
-- fact visible even without this fallback record.
attachCompilerResult :: CompilerResult -> [LineClassification] -> [LineClassification]
attachCompilerResult CompilerSucceeded cs = cs
attachCompilerResult result            [] =
  [ClassifiedOpaque (OpaqueGhcOutput StdErr BS.empty Text.empty (Just result))]
attachCompilerResult result            cs = map attach cs
  where
    attach (ClassifiedOpaque o)       = ClassifiedOpaque o { opaqueCompilerResult = Just result }
    attach d@(ClassifiedDiagnostic _) = d
```

**Definition of done:** invariant #19, #20 (§32) hold; §31.5's full test list passes, including the mixed-line and unexplained-non-zero-exit cases; `classifyStream` is exercised by a Hedgehog property asserting no input `ByteString` is ever dropped — the concatenation of every `opaqueRawBytes`/decoded-fragment recovers a byte-exact reconstruction of the input lines, including the blank line that terminates a panic block; `classifyInterleaved` is additionally exercised by a dedicated cross-stream property (I-13): for interleavings that place a panic-start line on one stream between two lines on the other, the completed records for the untouched stream appear in the output in exactly their input order, undisturbed by the pending panic, and the panic's own completed block appears at the position its terminating (or EOF-flushing) line occupied; a second property confirms `attachCompilerResult` never changes the `Maybe CompilerResult` on a `ClassifiedDiagnostic` and always sets the same `Just result` on every `ClassifiedOpaque` in its output for a given non-success `CompilerResult`; a third test confirms `buildCompilerResult` on the resulting `BuildOutcome` reflects a non-zero exit even when every captured line decoded as a valid diagnostic (I-17's acceptance gate).

---

## Phase 8 — Stream Semantics for the Pure Decoder (resolves §33 "line splitting, blank lines, CRLF, final unterminated line, collection APIs, per-record errors")

**Depends on:** Phase 2.
**Vision anchor:** §23 (Stream Semantics).

This is distinct from Phase 7: Phase 7 classifies *build tool* output (which may contain non-diagnostic lines by design); this phase defines the convenience collection API over a stream that is *known* to be GHC diagnostic JSON Lines (e.g. GHC invoked directly, or a file of captured diagnostics), where §23 asks for well-defined behaviour around blank lines and line endings without silently hiding which record failed.

```haskell
-- | Total. Blank lines are skipped (documented, not silent — this
-- function's Haddock states it); CRLF is normalized before decoding;
-- every surviving line keeps its own Either, so one malformed record
-- never obscures another's success or failure (§23: "must not obscure
-- which individual record failed").
decodeDiagnosticStream :: [ByteString] -> [Either DecodeError GhcDiagnostic]
decodeDiagnosticStream = mapMaybe decodeNonBlank . map stripTrailingCR
  where
    -- | Total unconditionally, not merely "safe because the guard makes
    -- it so": 'BS.take' is total for any 'Int', including a negative
    -- one, so this does not rely on 'BS.isSuffixOf' having already
    -- proven the input non-empty the way a guarded 'BS.init' would.
    -- 'BS.init' is deliberately not used here, since a function that is
    -- only safe because of an adjacent guard is exactly the pattern
    -- §0.1 bans, even when the guard happens to make it correct.
    stripTrailingCR bs
      | BS.isSuffixOf "\r" bs = BS.take (BS.length bs - 1) bs
      | otherwise             = bs
    decodeNonBlank bs
      | BS.null bs = Nothing
      | otherwise  = Just (decodeDiagnosticLine bs)
```

A final unterminated line (no trailing newline) is handled correctly by construction, because line splitting happens before this function is called (via a total splitter on `\n`, e.g. `ByteString.Char8.lines`-equivalent that does not require a trailing newline to recognize the last line) — this function never reads a handle itself, so there is no line-buffering edge case inside it to get wrong.

**Definition of done:** invariant #6 (§32) holds across stream boundaries, not just within one record; a property test confirms `length (decodeDiagnosticStream ls) <= length ls` with equality exactly when no line in `ls` is blank, for arbitrary generated `ls`.

---

## Phase 9 — Testing Infrastructure (resolves §33 "fixture corpus, property tests, malformed-input tests, schema-version tests, coordinate tests, Tadka integration tests, build-tool and opaque-output tests")

**Depends on:** all prior phases.
**Vision anchor:** §31 (all subsections).

### 9.1 Fixture layout

```
test/fixtures/
  schema/1.0/{minimal,maximal,missing-field,wrong-type,null-code,
              null-span,empty-message,empty-hints,unknown-fields}.json
  schema/1.1/{...same set...,optional-reason,reason-flags,reason-category}.json
  schema/1.2/{...same set...,rendered,missing-rendered}.json  -- POST-FREEZE: renamed from
                                              -- optional-rendered; rendered is required, not
                                              -- optional, under schema 1.2 (see Phase 1.3)
  coordinate/{first-char,same-line,multi-line,eol,final-char,empty-span,
              unicode,crlf,tabs,invalid,out-of-range}/{source.txt,span.json,expected.txt}
  render/message/{one-fragment,multi-fragment,empty}.golden       -- messageDoc, I-24
  render/help/{one-hint,multi-hint,empty}.golden                  -- helpDoc, I-24
  render/empty-message-diagnostic/{1.0,1.1,1.2}.golden            -- I-27, end-to-end
  build/{cabal-only,stack-only,ambiguous}/   -- project-file fixtures for detectBuildTool
  build/panic/{ghc-9.6,ghc-9.10,...}.txt     -- one captured real panic per supported GHC
                                              -- family in the compatibility matrix (§15),
                                              -- for isPanicMarker (I-14)
```

The `render/message/` and `render/help/` golden fixtures (I-24) go beyond the single captured example `messageDoc`'s `vsep` choice was originally justified by (§5.2): `one-fragment` and `multi-fragment` each fix the exact rendered text for a known input list (so a future change to the `vsep`/wrapping/separator choice is caught as a golden-file diff, not merely a passing property), and `empty` fixes `messageDoc [] = mempty`'s rendered output (the empty string) and `helpDoc [] = Nothing`'s absence of any rendered block, as distinct, explicit cases rather than being covered only incidentally by the property test in §9.3.

### 9.2 Test tooling

`tasty` as the runner; `tasty-golden` for fixture-file comparisons (§31.1, §31.2); `tasty-hunit` for the discrete Tadka-projection assertions (§31.3); `hedgehog` + `tasty-hedgehog` for the property layer (§31.4, §31.5), because Hedgehog's shrinking is better suited than QuickCheck's to the recursive, constructor-shaped generators this codebase's ADTs call for (a `GhcDiagnostic` generator, a `[ByteString]` build-output generator, etc.).

### 9.3 Property test list — one Hedgehog property per §31.4/§31.5 bullet, named for direct traceability

| Vision bullet (§31.4 / §31.5) | Property name |
|---|---|
| message fragment order preserved | `prop_messageDoc_preserves_order` |
| message fragments kept separate, not merged (§5.2) | `prop_messageDoc_preserves_fragment_count` |
| GHC code never becomes a Tadka code by accident | `prop_code_projection_always_Nothing` |
| GHC code stays reachable on `GhcDiagnostic` despite that non-projection (I-28) | `prop_ghcCode_preserved_through_projection` — asserts `ghcCode d == ghcCode d` is trivially not the point; concretely, that for arbitrary `d`, `Tadka.code d == Nothing` (from the row above) *and* `ghcCode d` is unchanged from the value `promote`/`promoteV1_X` originally placed there, so the two facts — "never fabricated into Tadka's `code`" and "never lost from `GhcDiagnostic` either" — are each independently exercised rather than one being assumed from the other |
| unsupported schema versions never silently decode | `prop_unsupported_version_always_Left` |
| source binding never performs implicit file I/O | enforced by construction (Phase 3.4) + `prop_bindSpan_uses_only_provided_provider` |
| parser behavior independent of Tadka rendering | `prop_decode_does_not_require_Tadka_instance` (type-level: `decodeDiagnosticLine`'s module has no `Tadka` import) |
| flag injection always present after injection | `prop_flag_always_present_after_injection` |
| flag injection idempotent | `prop_injection_idempotent` |
| no input line ever dropped by classification | `prop_classifyStream_reconstructs_input` |

**On what these properties may assert (I-32):** every property above, and every other property this specification defines, is stated over a *rendered* value (a `Text` produced by an explicit `render`/layout call, an `Either`/list/record of this codebase's own types) or over a documented stable representation — never over Prettyprinter's internal `Doc` constructor tree. `prop_messageDoc_preserves_fragment_count`, in particular, renders `messageDoc frags` to `Text` first (§5.2) and counts `\n`-delimited lines in that `Text`, specifically so the property keeps passing if Prettyprinter's own internal `Doc` representation changes between library versions; a property that pattern-matched on `Doc`'s constructors directly would be testing an implementation detail of a dependency, not a semantic contract this specification actually makes, which is exactly the class of test §31's own discipline (and I-32) rules out.

**No decode/encode round-trip property (I-33).** This specification defines no canonical encoder anywhere in its scope — `Tadka.GHCProtocol` exposes decoding (`decodeDiagnosticLine`/`decodeDiagnosticLineWith`/`decodeDiagnosticStream`) and Tadka projection, never a function that serialises a `GhcDiagnostic` back to GHC's wire JSON, and no phase in this document introduces one. A `decode . encode == id` property therefore has no encoder to be stated about and is correctly absent from this list; it is not an oversight, and the property list above intentionally contains only fixture- and structure-based decoder invariants instead (I-33's own required outcome). If a canonical encoder is ever added in a future revision, that revision must first define its exact serialization (which JSON shape it targets, and for which schema version) before any round-trip property referencing it is reintroduced — per this same section's discipline, such a property would itself need to be stated over rendered/serialised `Text`, not an internal representation.

### 9.4 Deterministic build-tool testing

`runBuild` (Phase 6.4) is tested against a **fake subprocess runner** — a `BuildRunner` capability record analogous to `SourceProvider` — so the full test suite never requires a real `cabal`/`stack`/GHC toolchain to be installed in CI for every test:

```haskell
newtype BuildRunner m = BuildRunner
  { execute :: Maybe NominalDiffTime -> BuildTool -> FilePath -> [Text] -> m BuildResult }
```

Production code supplies `ioBuildRunner :: BuildRunner IO` (backed by `runBuild`); tests supply a pure stub returning canned `BuildResult` values from the fixture corpus above. One narrow integration test (tagged separately, run in a dedicated CI job — Phase 10) exercises the real `ioBuildRunner` against a tiny scratch Haskell project, to catch drift between the fake and the real subprocess behaviour.

**Definition of done:** every bullet in §31.1–§31.5 has a corresponding, named, passing test; CI (Phase 10) fails the build if any test is skipped or pending.

---

## Phase 10 — CI and Packaging (resolves §33 "supported GHC matrix, schema fixture compatibility, warnings-as-errors policy, package bounds")

**Depends on:** all prior phases.

- **Warnings-as-errors:** already mandatory per §0.1, enforced in the `.cabal` file itself, not only in CI — CI additionally re-runs the build with `-Werror` explicitly passed as a defence-in-depth check against a local misconfiguration.
- **GHC support matrix:** made concrete in §15 below (I-36), rather than described only as a policy here — every supported cell names an exact GHC release, schema version, fixture path, and CI job, and a cell absent from that table is unsupported by definition, not merely untested.
- **Two CI jobs:** (a) the full unit/property suite against the fake `BuildRunner`, run on every GHC in the support matrix; (b) the narrow real-subprocess integration test (§9.4), run on the newest supported GHC only, to keep CI time bounded.
- **Package bounds:** PVP-style bounds on every dependency in §2's list, refreshed via a scheduled CI job that attempts a build against each dependency's latest release and opens an issue on failure, rather than bounds being hand-maintained only at release time. The `tadka` dependency bound additionally records the exact pinned release this specification's Phase 3.5/5.1 bridge was written and verified against, together with the exact list of `tadka` public modules/identifiers imported (`Tadka.Diagnostic`, `Tadka.Context`, `Tadka.NoContext`, `Tadka.mkNamedSource`, `Tadka.mkSpan`, `Tadka.Labeled`, `Tadka.Primary`, `Tadka.mkContext`, `Tadka.severity`/`Tadka.SevWarning`/`Tadka.SevError`, `Tadka.message`, `Tadka.help`, `Tadka.code`, `Tadka.related`, `Tadka.diagnosticId`, `Tadka.diagnosticCause`, `Tadka.url`) — per I-09, no future revision of this document may reintroduce an assumed Tadka signature without updating this record.
- **hlint:** run in CI as an additional check, non-blocking on style-only suggestions but blocking on any hint tagged as a correctness or partiality warning.

**Definition of done:** a tagged release only happens from a commit where both CI jobs are green on every GHC in the matrix.

---

## 11. Phase Dependency Graph

```text
Phase 0 (scaffolding)
   |
   +--> Phase 1 (schema/wire types) --> Phase 2 (semantic repr.) --> Phase 3 (span/coords)
   |                                          |                          |
   |                                          v                          v
   |                                    Phase 4 (errors) <----------------+
   |                                          |
   |                                          v
   |                                    Phase 5 (Tadka projection)
   |
   +--> Phase 6 (build tool wrapper)  [independent, parallelizable with 1-5]
              |
              v
        Phase 7 (opaque capture)  <-- depends on Phase 2's decodeDiagnosticLine
              |
   Phase 8 (pure stream semantics) <-- depends on Phase 2 only, parallelizable with 6-7
              |
              v
        Phase 9 (tests, all of the above)
              |
              v
        Phase 10 (CI/packaging)
```

Phases 1–5 (the decode/projection core) and Phase 6 (the build wrapper) have no dependency on each other and can be built by two engineers in parallel; Phase 7 is the join point where they meet.

---

## 12. Style Rules Not Yet Covered Above

- **No orphan instances.** `Tadka.Diagnostic GhcDiagnostic` and `Tadka.Diagnostic OpaqueGhcOutput` are defined in `Diagnostic.hs`, which is the module that owns both the class import and the type definitions' re-export — never bolted on from an unrelated module.
- **No `Read` instances** are derived for any type in this library; `Show` is for debugging output only, never parsed back (this is itself an instance of the "never reparse what was only meant to be displayed" principle §15 states for `RenderedDiagnostic`).
- **Every `Maybe`/`Either` returned by a public function is documented with what `Nothing`/`Left` means**, not left to be inferred from the type alone — a `Maybe GhcSpan` field and a `Maybe SourceText` result mean different things, and the Haddock says so explicitly at each site.
- **`StrictData`** is a mandatory default extension (§2), so every record field is strict by default rather than a lazily-deferred thunk — this removes one specific, common class of leak (fields silently accumulating unforced thunks through repeated record updates) structurally, without needing `NFData`/`deepseq` calls at each construction site. It does **not** eliminate space leaks in general: a lazy fold building up an unforced accumulator (e.g. a naive `foldr`/`foldl` over a long list), a lazily-retained list spine, or a `Left`/`Right` value holding an unforced thunk inside `Either` are all still possible and are guarded against individually at the specific sites in this specification where they could arise — for instance, `foldl'` (not `foldl` or a lazy fold) is used explicitly wherever this spec folds over a build's captured output (Phase 6.3, Phase 7.3) precisely because `StrictData` alone would not protect those folds.

- **Explicit per-subsystem memory properties (I-30), beyond the general `StrictData` caveat above:** `StrictData` addresses field thunks only; it is not, on its own, a bounded-memory guarantee for any of the four subsystems below, each of which is instead given its own explicit property, tested or benchmarked in Phase 9:
  - *Wire decoding (Phase 1–2):* a single JSON diagnostic line with an unusually large `message`/`hints` array or an unusually large `rendered` string decodes in memory proportional to that line's own size — `decodeRawByVersion`/`promote` make one traversal per field, never re-traversing or re-materialising a field already consumed — verified by a benchmark fixture (`test/fixtures/schema/*/maximal.json` sized up, or a generated large fixture) asserting decode time and peak residency scale linearly, not superlinearly, with input size.
  - *Coordinate conversion (Phase 3):* `computeLineMetadata`'s linear-scan property (I-12, already benchmarked per Phase 3.5's definition of done) is itself the source-text memory property — a large source file is walked once, producing one `LineMetadata` per line, not re-scanned per span converted against it.
  - *Long/unterminated output lines (Phase 6.5):* the accepted-and-documented unbounded-buffering policy for a pathological single line (I-20) is exercised by a dedicated stress test — a very large unterminated line — that confirms the line is fully captured (not truncated) and that memory use during accumulation is proportional to the line's own length, not to some larger multiple of it from an accidental quadratic append pattern in `feedChunk`/`splitCompleteLines`.
  - *Long build output (Phase 6–7):* a build producing a large number of captured lines (many short diagnostics and/or many opaque lines) is exercised by a benchmark confirming `classifyInterleaved`/`classifyBuildOutput`'s cost is proportional to the number of lines, consistent with their stated structural-recursion totality arguments, and that the shared output buffer `runBuild` appends to (§6.4 step 2) is built with a strict, amortised-append structure rather than a data structure whose repeated append is itself quadratic.
  
  These four are the concrete content behind "memory tests/benchmarks cover large JSON, source text and long output lines" (§6 of the fix register's acceptance matrix); none of them is claimed to follow automatically from `StrictData` alone.

---

## 13. Explicit Non-Goals of This Specification

Matching the vision document's own discipline (§1's "is not" list), this spec is explicitly **not**:

- a specification of the exact JSON field names GHC's schema uses (those must be confirmed against real GHC output during Phase 1/9 fixture collection, not guessed here);
- a specification of Tadka's own internal implementation of `Span`/`Context`/`mkSpan`/`mkNamedSource`/`mkContext` (owned by the `tadka` package; this specification consumes only their public signatures, per §6, and records the pinned `tadka` release and exact public imports used in Phase 10's dependency bounds, per I-09 — it does not restate or duplicate that package's own documentation);
- a decision about CLI flag naming or executable UX beyond "a `tadka-ghc` entry point exists that wraps Phase 6" — that belongs in a separate CLI-design note if one is needed.

---

## 14. Consistency Checklist — Traceability Against the Vision Document

Status legend (I-37: four statuses, so nothing externally unverified is ever labelled as if it needed no further check): **Verified** — backed by GHC's own published schema, a real captured example, or the real public `tadka` package API (Phase 1.3, Phase 9's Tadka verification), not merely asserted. **Design-fixed** — a design decision made entirely by this specification, complete and not contingent on any external fact (GHC's behaviour or a third-party API). **External verification required** — this specification's stated, complete design, whose correctness nonetheless depends on a fact about GHC's real output or another external system that has not yet been checked against a pinned fixture; the design itself is not an open question, only its match to reality is unconfirmed. **Blocked** — implementation freeze may not proceed on this item until the "external verification required" check above is actually performed and recorded (I-08); this specification does not treat a row in this state as ready to build against.

| Vision §33 open item | Resolved in | Status |
|---|---|---|
| exact `SchemaVersion` | Phase 1.1 | Design-fixed |
| exact `GhcVersion` | Phase 2.1 | Design-fixed |
| exact `GhcSpan` | Phase 3.3 | Design-fixed (field shapes now built from the confirmed nested wire span, Phase 1.3) |
| exact `GhcSeverity` | Phase 2.3 | **Verified** — GHC's own schema enumerates exactly `"Warning"`/`"Error"` |
| exact `GhcDiagnosticCode` | Phase 2.2 | Design-fixed |
| exact `DiagnosticReason` | Phase 2.4 | **Verified** for the two-shape `oneOf` structure (`flags`/`category`); the schema-version number it first appears under (1.1) is confirmed, per Phase 1.3 |
| exact `RenderedDiagnostic` | Phase 2.4 | Design-fixed |
| exact `GhcDiagnostic` | Phase 2.5 | Design-fixed |
| exact Aeson raw types | Phase 1.3 | **Verified** for the 1.0/1.1 field set and the nested `span`/`start`/`end` shape; **Verified** (POST-FREEZE) for the schema-version label (`"1.2"`) the `rendered` field first appears under, against GHC's own #26173 commit — and this same verification uncovered that `rendered` is **required**, not optional, under 1.2, a genuine specification error corrected in the implementation (see Phase 1.3's update) |
| schema dispatch | Phase 1.2, 1.4 | Design-fixed |
| decoding mode (strict vs. forward-compatible) as a concrete API | Phase 2.7 (`DecodingMode`, `DecodeWarning`, `decodeDiagnosticLineWith`) | Design-fixed — added in this revision; `decodeDiagnosticLine`'s original type is unchanged |
| validation rules | Phase 1.4, 2.6 | Design-fixed |
| strict vs. forward-compatible unknown-field behaviour, and unknown fields remaining inspectable | Phase 2.7 | Design-fixed — `ForwardCompatible` mode reports every unconsumed key, together with its own raw JSON value (I-25), as a `DecodeWarning` rather than only leaving it inspectable via the raw object |
| numeric bounds / nullability / optional fields | Phase 1.3 (`Integer`, not `Int`, at the wire boundary), Phase 2.2, Phase 3.2 (`mkLine`/`mkColumn` now bounds-checked against `Int`, the domain every downstream offset computation actually uses — I-11) | Design-fixed |
| exact transformation `[Text] -> Doc Ann`, and its justification | Phase 5.2 (`messageDoc`) | Design-fixed — fragment semantics and the `vsep` choice are now stated explicitly, not merely asserted; verified beyond the single original captured example by dedicated one/multiple/empty-fragment golden fixtures (I-24), and the empty-message case is additionally confirmed end-to-end distinguishable through real Tadka rendering (I-27) |
| exact transformation `[Text] -> help` | Phase 5.2 (`helpDoc`) | Design-fixed |
| source-provider abstraction | Phase 3.4 | Design-fixed — `lookupSource` now returns `Either SourceLookupError (Maybe SourceText)`, distinguishing a lookup failure from a clean not-found (I-06) |
| source identity / file lookup policy | Phase 3.4, 3.5 | Design-fixed |
| missing-source behaviour | Phase 3.5 (`SpanNoSource`, `SpanSourceUnavailable`, both non-error) | Design-fixed |
| coordinate conversion, including a genuine (not `undefined`) `convertSpan` | Phase 3.1, 3.2, 3.5 (`computeLineMetadata`, `coordinateToOffset`, `convertSpan`) | Endpoint convention: **Verified** by a real example. One-based/code-point/CRLF/tab conventions: **External verification required** — and, per I-08, this row is **Blocked** for implementation freeze until that verification against pinned real GHC fixtures is actually performed and recorded during Phase 9; the CRLF *design* itself (excluding the CR from the preceding line's column range) is Design-fixed (I-07), only its match to every supported GHC's real behaviour remains to be checked. `Tadka.mkContext`'s signature is no longer an open item: it is now the real, public four-stage pipeline (`mkNamedSource`/`mkSpan`/`Labeled`/`mkContext`, I-02), which is Design-fixed against the actual API rather than assumed. |
| stale-source behaviour | Explicitly deferred (Phase 3.5), matching vision §18's amendment | Design-fixed (deferral is itself the resolution) |
| multi-source context construction | Phase 3.3 (`spanFile` preserved per diagnostic; vision §20 notes multi-location support is a future protocol extension, not a v1 requirement) | Design-fixed |
| exact `Diagnostic` instance, including how it receives span/context data | Phase 5.1 (`GhcDiagnostic` and the new `BoundGhcDiagnostic`), 5.3 | Design-fixed — rewritten against the real `Tadka.Diagnostic` class, which requires `context :: e -> Tadka.Context` (not `Maybe Tadka.Context`) and a `url` method this specification had previously omitted entirely (I-01/I-09); `BoundGhcDiagnostic`'s delegated methods are now explicitly `Tadka.`-qualified to remove any risk of accidental self-reference (I-03) |
| exact `Context` construction | Phase 3.5 (`convertSpan`), consumed by `BoundGhcDiagnostic`'s `context` (Phase 5.1) | Design-fixed against the real `tadka` public API (I-02) |
| exact severity mapping | Phase 5.1 | **Verified** by GHC's schema (see `GhcSeverity` above) |
| exact treatment of code/reason/rendered | Phase 5.1 (`code`), Phase 2.6 (`reason`/`rendered` version-availability, raw-preservation and no-reparsing rules stated in full, I-29; `ghcCode`'s continued reachability on `GhcDiagnostic` alongside `Tadka.code`'s permanent `Nothing`, I-28) | Design-fixed — POST-FREEZE: the required-not-optional correction to `rendered`'s wire parsing (Phase 1.3) does not change this row's own design, since `ghcRendered`'s `Maybe` at the `GhcDiagnostic` level was already and remains correct for the reason stated at Phase 2.6's update |
| exact error boundaries | Phase 4 | Design-fixed — `DecodeMissingField`/`DecodeFieldTypeMismatch` now carry the schema version and a structured `JsonPath` to the enclosing object, not just a bare field name (I-26) |
| line splitting / blank lines / CRLF / final unterminated line | Phase 8 | Design-fixed (`stripTrailingCR` now total unconditionally, not merely under its guard) |
| collection APIs / per-record errors | Phase 8 | Design-fixed |
| Cabal exposed modules / internal modules / dependencies / compatibility policy | Phase 2 (module layout), Phase 10 | Design-fixed — module layout reconciled against every type introduced since the original sketch (`BoundGhcDiagnostic`, `DecodingMode`/`DecodeWarning`, `SourceLookupError`/`SpanState`/`SourceBindingError`, `CompilerResult`/`Signal`/`ProcessError`/`BuildResult`, `BuildOutcome`) and each placed in the module that owns it (I-34); the process-execution boundary is additionally split into its own component per I-35 below |
| fixture corpus / property tests / malformed-input / schema-version / coordinate / Tadka integration tests | Phase 9 | Design-fixed |
| build-tool wrapper: detection, flag injection, precedence, subprocess capture, exit codes, timeout | Phase 6 | Design-fixed — flag detection now parses actual GHC flag tokens with an explicit last-occurrence-wins precedence rule, rather than a substring search (I-22); detection now specifies search root/depth, recognised files, precedence and an explicit override parameter (I-21); process outcome is now the vision's own richer `CompilerResult` model (`CompilerSucceeded`/`CompilerFailed`/`CompilerSignalled`/`CompilerTimedOut`/`CompilerStartFailed`, I-16/I-17/I-18/I-23), and `runBuild`'s full spawn/drain/timeout/terminate/cleanup lifecycle is now specified step by step rather than left as a bare contract (I-05) |
| chunk-to-line framing over raw subprocess pipe reads | Phase 6.5 | Design-fixed — added in this revision; not covered by the original spec at all; chosen resource-bound policy is now stated explicitly rather than left implicit (I-20) |
| core (pure GHC adaptation) vs. CLI/process-execution package boundary | Phase 2 (module layout), Phase 6 (component note) | Design-fixed — `runBuild`'s actual process I/O now lives in a separate `tadka-ghc-process` component (`Runner.hs`), depended on by the `tadka-ghc` executable only; the core `tadka-ghc` library (detection, flag injection, framing, decoding, projection, classification) has no dependency on `typed-process` or any process-spawning capability (I-35) |
| cross-stream interleaving guarantee, stated accurately | Phase 6.1, 7.3 | Design-fixed — now described as "observed order," not "production order" (I-19); the two-stream classifier (`classifyInterleaved`) is now a complete, independent-per-stream state machine with a stated EOF-flush order, not merely a type signature (I-04/I-13) |
| opaque output: exact type, grouping heuristic, minimal `Diagnostic` instance, encoding handling | Phase 7 | Design-fixed — `OpaqueGhcOutput` now separates `opaqueRawBytes` (genuinely raw, payload-only — I-15) from `opaqueText` (a lenient rendering); this is a refinement of the vision's §4 sketch, recorded here rather than silently substituted |
| exit-code attachment semantics for opaque records | Phase 7.4 (`attachCompilerResult`, `BuildOutcome`) | Design-fixed — now a uniform post-processing pass over every record, not an ad hoc synthetic-record-only case; the whole build's `CompilerResult` is additionally exposed at the top level (`buildCompilerResult`) so it can never disappear behind successfully-decoded records (I-16/I-17) |
| panic-block reconstruction completeness (no line dropped, including delimiters) | Phase 7.3 | Design-fixed — the panic-terminating blank line is now folded into the block instead of being discarded; delimiter semantics for `opaqueRawBytes` are now stated explicitly as payload-only, with exact stream reconstruction defined at the framed-record layer (I-15) |
| exact GHC panic banner string (`isPanicMarker`) | Phase 7.3 | Matching rule (prefix, not exact-line match; how alternate/family-specific banner forms are grouped) is Design-fixed (I-14); the literal prefix's match against every supported GHC family remains **External verification required**, recorded per family in §15's compatibility matrix — POST-FREEZE: the literal `"ghc: panic!"` prefix is now **Verified** directly against `GHC.Utils.Panic.Plain`'s `Show GhcException` instance (`progName ++ ": " ++ panicMsg`, `progName = "ghc"`), confirmed present unchanged in `ghc-9.14.1`, and corroborated by real captured transcripts spanning GHC 6.7 through 9.4.4. A genuine, honestly-documented limitation was found in the process and is NOT claimed as solved: a panic re-routed through GHC's own diagnostic-rendering pipeline can instead appear as a separate `"<no location info>: error:"` line followed by an indented `"panic!"` line with no `"ghc: "` prefix, which `isPanicMarker` does not recognise as a block start. Per §4's completeness guarantee this causes no data loss — the unrecognised lines still become individual `OpaqueGhcOutput` records via the `Idle`-state fallback — only worse grouping (many single-line records instead of one block). Tested explicitly in `test/Main.hs` as a documented known-limitation case. |
| supported GHC matrix / schema fixture compatibility / warnings-as-errors / package bounds | Phase 10, §15 (I-36) | Design-fixed for the table's structure and policy; individual cells are **External verification required** / **Blocked** exactly where §15 itself says so (e.g. the 9.6 schema cutover and the 10.0+ `"1.2"` label) — POST-FREEZE: the `"1.2"` *label* itself is now Verified (see above), though the row remains Blocked for a fixture; additionally, real CI (`ci.yml`) targets 9.10.3/9.12.4/9.14.1, not the 9.4.x/9.6.x/9.10.x/≥10.0 families this table was originally drafted against, and GHC 9.14.1 has been empirically confirmed (via a real `cabal build --ghc-options=-fdiagnostics-as-json` run, not a fixture) to emit schema `"1.1"`. §15's table below has been updated with a dedicated row for this. |

Every row above resolves an item the vision document explicitly left open; no row contradicts an explicit rule stated elsewhere in the vision document — cross-checked against §1 (non-goals), §11 (no Advice severity), §12 (empty message array must be accepted), §18/§1 (stale-span deferred as an editor concern), and §32's twenty architectural invariants, all of which this specification's phases satisfy by the "definition of done" stated in each. Rows marked **External verification required** are not open design questions this specification failed to resolve — the *design* is fixed in each case — they are facts about GHC's real behaviour that this specification states as its working assumption and flags for confirmation, rather than silently guessing and presenting the guess as verified; no row in this table is marked **Verified** on the strength of an assumption alone, and no row that still requires such confirmation is left labelled as though it were **Design-fixed** and therefore ready to build against without further checking. Per I-08 and I-37, no item in this table may be marked **Verified** merely because this specification describes a resolution — it is Verified only once checked against the pinned real GHC/`tadka` fixtures Phase 9 collects; until then, the corresponding implementation-freeze gate remains **Blocked** exactly where this table says so.

---

## 15. GHC / Schema / Fixture / CI Compatibility Matrix (resolves I-36)

§10's "GHC support matrix" and §14's "exact `SchemaVersion`"/coordinate-convention rows both depend on this table being concrete rather than a policy statement. Each row names one supported GHC release family, the schema version it is expected to emit, the fixture directory that backs it, the specific facts that fixture must confirm, and the CI job that exercises it. A GHC family with no row here is, by this table's own definition, **unsupported** — not "supported but unverified" — consistent with I-08's freeze rule that an unverified cell blocks freeze rather than being silently assumed to pass.

| GHC release family | Schema version | Fixture source | Fields/facts this cell must confirm | CI job |
|---|---|---|---|---|
| GHC 9.4.x | `"1.0"` | `test/fixtures/schema/1.0/*`, `test/fixtures/coordinate/*` (run against a 9.4-produced source/span pairing), `test/fixtures/build/panic/ghc-9.4.txt` | Top-level field set (§1.3) with no `reason`/`rendered`; one-based/code-point/CRLF/tab coordinate conventions (§3.1); `isPanicMarker`'s literal prefix (I-14) | Unit/property suite (§9.4 job (a)), this GHC version |
| **GHC 9.14.1 (POST-FREEZE addition)** | `"1.1"` | `test/fixtures/schema/1.1/*`, real `cabal build --ghc-options=-fdiagnostics-as-json` output captured during manual CLI QA | **Empirically confirmed**, not a fixture assumption: a real build against this exact installed GHC emitted `"version":"1.1"` with no `rendered` field, consistent with that binary predating ticket #26173. `isPanicMarker`'s literal prefix independently Verified against this same GHC's own `GHC.Utils.Panic.Plain` source. This is the actual GHC version this codebase has been built, tested (140+ tests), and manually QA'd against; it is not in the original 9.4.x/9.6.x/9.10.x/≥10.0 matrix and is added here rather than silently left unreconciled against `ci.yml`'s real 9.10.3/9.12.4/9.14.1 matrix. | Full suite + real-subprocess integration, this exact environment |
| GHC 9.6.x | `"1.0"` or `"1.1"` (exact minor-version cutover to be confirmed) | as above, plus `test/fixtures/schema/1.1/*` if 9.6 is confirmed to emit 1.1; `test/fixtures/build/panic/ghc-9.6.txt` | Same as GHC 9.4.x row; additionally, if schema 1.1: `reason`'s two-shape `oneOf` (§1.3) | Unit/property suite (§9.4 job (a)), this GHC version |
| GHC 9.10.x | `"1.1"` | `test/fixtures/schema/1.1/*`, `test/fixtures/coordinate/*`, `test/fixtures/build/panic/ghc-9.10.txt` | `reason` field present and matching the confirmed `oneOf` shape (§1.3); coordinate conventions re-confirmed for this release specifically, not assumed carried over from 9.4.x | Unit/property suite (§9.4 job (a)); **also** the narrow real-`ioBuildRunner` integration job (§9.4 job (b)), since this is the newest-supported release per §10's policy |
| GHC ≥10.0 (post `-fdiagnostics-as-json` `rendered` change, ticket #26173) | `"1.2"` (provisional label — I-08/Phase 1.3) | `test/fixtures/schema/1.2/*`, `test/fixtures/coordinate/*`, `test/fixtures/build/panic/ghc-10.x.txt` | Whether `rendered` is actually introduced under the `"1.2"` label specifically (Phase 1.3's still-open point); `rendered`'s raw-preservation rule (I-29) against a real rendered string; coordinate conventions re-confirmed | Unit/property suite (§9.4 job (a)) once the schema-version label is confirmed; **blocked** (§14) until then |

Every cell's fixture directory is exercised by the fake-`BuildRunner` unit/property suite (§9.4) on every push; only the newest supported release additionally runs the real-subprocess integration job, per §10's "two CI jobs" policy, so CI time stays bounded as this table grows. Adding a new supported GHC release is, by construction, "add a row to this table plus its fixture directory" — never a change to any type or algorithm elsewhere in this specification, since every phase upstream of Phase 9 is already written against the closed `SchemaVersionTag`/`GhcSeverity`/etc. universe (§1), not against a GHC version number directly. A row whose "exact ... to be confirmed" language has not yet been resolved is exactly the case §14's **External verification required**/**Blocked** statuses exist for, and such a row must not be treated as ready to ship against until Phase 9 resolves it and this table is updated to state the confirmed fact plainly.
