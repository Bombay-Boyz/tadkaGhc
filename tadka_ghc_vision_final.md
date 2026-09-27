# Tadka-GHC — Vision and Architecture

## 1. Purpose

`tadka-ghc` is an adapter for GHC's external JSON diagnostics protocol.

Its purpose is to:

1. run a project's build (via `cabal build`/`stack build`) with GHC's diagnostics-as-JSON protocol always enabled, so the user never has to remember a flag;
2. decode GHC diagnostics faithfully;
3. expose those diagnostics as useful GHC-domain data;
4. capture any build output that is not a decodable GHC diagnostic — panics, crashes, non-GHC build-tool failures — as an explicit, clearly-labeled record rather than silently discarding it;
5. optionally project both the decoded diagnostics and the captured records into Tadka's diagnostic model;
6. let Tadka's existing renderers handle graphical, prose, and JSON output.

The central principle is:

> **Decode GHC faithfully first. Adapt to Tadka second. Never distort GHC data merely to make it fit Tadka.**
>
> **Capture every failure the build produces, structured or not — but never blur the line between what was faithfully decoded and what was merely captured.**

`tadka-ghc` is not:

- a replacement for GHC diagnostics;
- a second diagnostic renderer;
- a wrapper around GHC's internal compiler API (it invokes the external `cabal`/`stack` build process, not GHC's library API);
- an LSP implementation;
- an editor integration layer;
- a general logging library.

---

## 2. Architectural Shape

```text
cabal build / stack build
 (tadka-ghc injects -fdiagnostics-as-json)
          |
          v
 combined build output, captured line by line
          |
          v
     line classification
          |
     +----+-----------------------+
     |                            |
     v                            v
valid GHC diagnostic JSON   everything else
     |                     (panics, crashes,
     v                      non-GHC failures,
schema-specific JSON        malformed lines)
     |                            |
     v                            v
decoded GHC diagnostic     OpaqueGhcOutput
     |                            |
     v                            |
 GhcDiagnostic                    |
     |                            |
     +--------------+             |
     |              |             |
     v              v             v
arbitrary      source binding  direct, minimal
consumers            |         projection
                      v             |
              Tadka Diagnostic <----+
                      |
                      v
                 Tadka 2.x
               /      |      \
        graphical   prose    JSON
```

There are deliberately three boundaries:

### Capture boundary

Build tool process output -> `GhcDiagnostic` (via decode) or `OpaqueGhcOutput` (via passthrough)

### Protocol boundary

GHC JSON -> `GhcDiagnostic`

### Tadka boundary

`GhcDiagnostic` + optional source material -> Tadka `Diagnostic`, or `OpaqueGhcOutput` -> Tadka `Diagnostic`

This separation is fundamental. Parsing must not require Tadka rendering, and a GHC diagnostic must remain useful even when no source text is available. The capture boundary is equally fundamental: classifying output as decodable-or-not must not require Tadka rendering either, and must never collapse the two outcomes into one type.

---

## 3. Build Tool Invocation

`tadka-ghc` owns the invocation of the project's build, rather than requiring the user to pass a flag to `ghc` themselves — most Haskell users never invoke `ghc` directly, so a flag on `ghc` alone would not be seen in practice.

Concretely:

- The `tadka-ghc` entry point (CLI or library call) shells out to the project's build tool — `cabal build` or `stack build` — exactly as the user would otherwise invoke it.
- It injects GHC's diagnostics-as-JSON flag via the build tool's own flag-forwarding mechanism (e.g. `--ghc-options=-fdiagnostics-as-json`), or via a `cabal.project`/`stack.yaml` override where that proves more reliable, so the flag reaches GHC for every module compiled without the user ever typing it.
- The user's own `--ghc-options` and other build-tool flags must still be respected; `tadka-ghc` must add its flag without removing or overriding flags the user, or their project file, already supplies.
- Build-tool selection (cabal vs. stack) is detected from the project (e.g. presence of `*.cabal`/`cabal.project` vs. `stack.yaml`), with an explicit override available rather than relying on detection alone.

This is a deliberate, narrow process boundary: `tadka-ghc` wraps the external build-tool process. It still does not link against or call into GHC's internal compiler API — GHC remains a subprocess spawned by cabal/stack, not a library dependency of `tadka-ghc` (§1).

---

## 4. Unstructured / Opaque Output Handling

Not everything a build produces is a GHC diagnostic. `cabal build`/`stack build` output may also contain GHC panics and internal errors, RTS crashes and out-of-memory output, dependency-resolution failures, `Setup.hs`/custom-setup failures, and the build tool's own progress or error text.

None of this has a JSON schema to decode against, so it must not be forced through `decodeDiagnosticLine` (§22) or fabricated into a `GhcDiagnostic` — doing so would violate the same "never distort data to make it fit" principle that governs GHC's own JSON (§1).

Instead, captured build output is classified per line (or per contiguous block, for multi-line panics):

- a line that decodes successfully as GHC diagnostic JSON becomes a `GhcDiagnostic` via the existing decode path (§22);
- everything else — malformed JSON, plain text, a non-zero exit with no diagnostics at all — is preserved verbatim as:

```haskell
data OpaqueGhcOutput = OpaqueGhcOutput
  { opaqueSource   :: OutputStream      -- stdout | stderr
  , opaqueText     :: Text              -- raw, unmodified
  , opaqueExitCode :: Maybe ExitCode    -- populated once the process exits
  }
```

`OpaqueGhcOutput` is deliberately a distinct type from `GhcDiagnostic`. It is not "decoded" — it carries no span, no severity, no code, because none of that was genuinely present in the input. It still gets a minimal Tadka `Diagnostic` projection (severity defaults to error, message is the raw captured text rendered as a literal block, no span/help/code), so nothing the build produced is ever silently dropped — but that projection must never be presented as though it were a faithfully parsed GHC diagnostic.

This is the completeness guarantee: every line of build output becomes either a faithfully decoded `GhcDiagnostic` or an honestly-labeled `OpaqueGhcOutput`. Nothing is discarded, and nothing is invented.

---

## 5. Relationship to Existing Tadka GHC Interop

Tadka already contains `Tadka.Interop.GHC`.

That module solves a different problem:

```text
GHC compiler API
    SrcSpan
       |
       v
Tadka Span
```

`tadka-ghc` solves:

```text
GHC external JSON protocol
       |
       v
GhcDiagnostic
       |
       v
Tadka Diagnostic
```

These should remain separate.

The JSON adapter must not be implemented by pretending that JSON is merely another representation of `SrcSpan`. The protocol contains information that `SrcSpan` does not contain, including diagnostic severity, code, message fragments, hints, reason, and—depending on schema version—rendered output.

---

## 6. Tadka API Boundary

The implementation must respect Tadka's established public/private boundary.

The supported Tadka application-facing API is exposed through:

```haskell
import Tadka
```

`Tadka.Internal.*` modules are implementation details and carry no compatibility guarantee.

Therefore `tadka-ghc` must not make its public architecture depend on:

- `Tadka.Internal.SourceCode`;
- `Tadka.Internal.Span`;
- `Tadka.Internal.Context`;
- other `Tadka.Internal.*` representations.

Where a public Tadka constructor or operation exists, use it.

In particular, the adapter should reuse Tadka's existing:

- `Span`;
- `NamedSource`;
- `Context`;
- `Labeled`;
- `LabelKind`;
- `Severity`;
- `Diagnostic`;
- `DiagnosticCode` only where its semantics genuinely apply;
- `Doc Ann` through the `Diagnostic` interface.

These are the public `Span` and `Context` types as re-exported through `import Tadka`. Only that public import path is sanctioned — not `Tadka.Internal.Span` / `Tadka.Internal.Context` — even though the type names coincide with the internal modules listed above.

This keeps `tadka-ghc` coupled to the supported Tadka abstraction rather than to implementation details.

---

## 7. Supported GHC JSON Schemas

The initial implementation targets:

- diagnostics JSON schema 1.0;
- diagnostics JSON schema 1.1;
- diagnostics JSON schema 1.2.

The implementation must treat the schema version as protocol data.

Conceptually:

```haskell
newtype SchemaVersion = SchemaVersion Text
```

The public representation should not imply that the currently supported versions are the permanent universe of possible versions.

Internally, decoding may dispatch through known versions:

```text
SchemaVersion
    |
    +-- 1.0
    +-- 1.1
    +-- 1.2
    +-- future version
```

Unsupported versions must fail explicitly rather than being silently interpreted as a known schema.

---

## 8. Schema-Specific Wire Types

Do not represent all schema versions as one giant record containing every possible field as `Maybe`.

Prefer schema-specific raw representations:

```haskell
data RawV1_0 = ...
data RawV1_1 = ...
data RawV1_2 = ...
```

This makes version-specific rules explicit and prevents accidental acceptance of fields that did not exist in a particular schema.

The raw layer is an implementation concern. It should not become the main public semantic API.

---

## 9. Stable Semantic Representation

After schema validation, promote the wire representation into a stable GHC-domain type.

Conceptually:

```haskell
data GhcDiagnostic = GhcDiagnostic
  { ghcVersion   :: GhcVersion
  , ghcSpan      :: Maybe GhcSpan
  , ghcSeverity  :: GhcSeverity
  , ghcCode      :: Maybe GhcDiagnosticCode
  , ghcMessage   :: [Text]
  , ghcHints     :: [Text]
  , ghcReason    :: Maybe DiagnosticReason
  , ghcRendered  :: Maybe RenderedDiagnostic
  }
```

Here `ghcVersion` identifies the GHC compiler that produced the diagnostic (e.g. `9.10.1`) and is distinct from `SchemaVersion` (§7), which identifies the JSON protocol version. `GhcVersion` is genuinely part of the diagnostic's own content; it is not the field discussed in the next paragraph.

The schema version is protocol/envelope metadata rather than part of the diagnostic's intrinsic identity. If it is useful to callers, expose it through the parsed envelope or equivalent protocol metadata rather than making it semantically equivalent to the diagnostic itself.

The important distinction is:

```text
Wire/protocol metadata
        |
        v
GHC diagnostic semantics
        |
        v
Tadka semantic projection
```

---

## 10. GHC Diagnostic Code

GHC's diagnostic `code` is an integer or `null`.

Tadka's `DiagnosticCode` has its own grammar and semantics:

```text
^[a-z][a-z0-9_]*::E[0-9]{4,}$
```

Therefore a GHC numeric code must not be fabricated into a Tadka code such as:

```text
ghc::E0564
```

That would falsely imply that the GHC code is a Tadka diagnostic code.

Instead:

```haskell
newtype GhcDiagnosticCode = GhcDiagnosticCode Int
```

preserves the GHC value.

For the base Tadka adapter, Tadka's:

```haskell
code :: e -> Maybe DiagnosticCode
```

should return `Nothing` unless a future, explicitly defined convention establishes a genuine Tadka code mapping.

The original GHC code remains available through `GhcDiagnostic`.

---

## 11. Severity

GHC's JSON schema defines:

- `Warning`
- `Error`

Tadka defines:

- `SevAdvice`
- `SevWarning`
- `SevError`

The direct mapping is:

```text
GHC Warning -> Tadka SevWarning
GHC Error   -> Tadka SevError
```

There is no `Advice` mapping.

The adapter must not invent one.

If a supported schema version reports a severity value outside `Warning`/`Error`, decoding must fail explicitly rather than defaulting to one of the two, consistent with the unsupported-version handling in §7/§26. Any future severity value requires an explicit spec update, not a silent guess.

---

## 12. Message Fragments

GHC represents `message` as an ordered array of strings.

The GHC schema does not impose a non-empty-array constraint. Therefore the decoder must not incorrectly reject an empty message merely because ordinary diagnostics normally contain text.

The GHC-domain representation remains:

```haskell
ghcMessage :: [Text]
```

The Tadka bridge must then deliberately transform the ordered fragments into Tadka's:

```haskell
message :: e -> Doc Ann
```

The implementation specification must define this transformation explicitly.

The invariant is:

> **Preserve fragment order and do not introduce semantic information that was not present in the GHC message.**

The implementation must not casually choose an arbitrary separator merely because `Text.intercalate "\n"` is convenient.

---

## 13. Hints

GHC's `hints` are ordered suggested fixes.

They may be projected into Tadka's `help`, but this is a semantic mapping rather than a wire-format identity.

The adapter must:

- preserve hint ordering;
- preserve the complete textual content;
- distinguish individual hints clearly;
- avoid silently discarding hints.

The exact `Doc Ann` structure should be defined by the implementation specification rather than hard-coded into the vision.

---

## 14. Reason

Schema 1.1 and later may contain a `reason`.

The semantic representation should preserve the distinction between the two schema forms:

```haskell
data DiagnosticReason
  = ReasonFlags (NonEmpty Text)
  | ReasonCategory Text
```

Reason must not be collapsed into:

- severity;
- help;
- message;
- diagnostic code.

Tadka currently has no corresponding semantic field, so the base adapter should preserve `reason` in `GhcDiagnostic` without inventing a Tadka field for it.

---

## 15. Rendered Diagnostic

Schema 1.2 may contain `rendered`.

This field must be preserved exactly as supplied by GHC.

It is **not** input to Tadka's renderer.

The distinction is:

```text
GHC rendered
     |
     +--> preserved for consumers
     |
     X--> not reparsed as Tadka structure
     X--> not passed through Tadka rendering
```

This prevents double rendering and loss of information.

---

## 16. Source and Span Model

GHC's JSON span identifies:

```text
file
start line/column
end line/column
```

Tadka's `Span` is based on character offsets and can subsequently be resolved against `NamedSource`.

The adapter should therefore:

1. preserve the GHC file identity;
2. obtain source text explicitly;
3. convert GHC coordinates into Tadka's offset-based span;
4. let Tadka resolve/render the span using its public API.

Do not duplicate Tadka's offset or span abstractions.

---

## 17. Source Binding Is a Separate Architectural Operation

Parsing a diagnostic and binding it to source are distinct operations.

A parsed diagnostic can exist without source text.

Conceptually:

```text
             GhcDiagnostic
                  |
        +---------+---------+
        |                   |
        v                   v
 no source binding     source binding
        |                   |
        v                   v
GHC-domain consumer   Tadka Diagnostic
```

The source provider must therefore be explicit.

The library must not silently read arbitrary files from disk merely because a GHC diagnostic contains a file name.

The implementation specification must define an explicit source-provider abstraction or equivalent API.

This also keeps the parser pure and independently useful.

---

## 18. Span State Must Be Explicit

The implementation must distinguish at least these situations:

| GHC/source situation | Meaning |
|---|---|
| `span = null` | Diagnostic has no source span |
| Span present, source unavailable | Coordinates exist but cannot be bound to source |
| Span present, source available, coordinates invalid | Input/source inconsistency |
| Span present, source available and valid | Construct Tadka span/context |
| Span becomes invalid against changed source | Tadka stale-span semantics may apply |

These cases must not collapse into a generic "no span" state.

In particular, absence of source material is not the same thing as a malformed GHC coordinate.

The last row concerns a diagnostic being resolved against source that has changed since the diagnostic was produced — a long-running or editor-like consumption pattern. Per §1, `tadka-ghc` is not an editor integration layer, so this row is listed only so the representation doesn't foreclose that use case; it is not a required v1 behavior.

---

## 19. Coordinate Conversion

GHC line/column coordinates and Tadka offsets use different representations.

The implementation must explicitly define:

- one-based versus zero-based coordinates;
- whether columns are character/code-point based;
- inclusive versus exclusive endpoints;
- conversion at end-of-line;
- newline handling;
- CRLF handling;
- tabs;
- Unicode code points;
- zero-length spans;
- out-of-range coordinates.

The existing Tadka GHC interop implementation is useful precedent for coordinate conversion, but `tadka-ghc` must not depend on its internal implementation.

The final implementation should use Tadka's public `mkSpan` and related public APIs rather than duplicating Tadka's span representation.

---

## 20. Multi-Source Diagnostics

Tadka 2.x supports multi-source context.

The GHC JSON span itself identifies one source file per diagnostic.

The adapter should preserve that file identity rather than flattening it into a basename or discarding it.

If future GHC protocol information supplies multiple source locations, the architecture should be able to represent them without changing the fundamental source-binding model.

For the current schema, a single GHC span should result in a single source group when source material is available.

---

## 21. Related Diagnostics, Causes, and IDs

The GHC JSON schemas do not provide Tadka's:

- related diagnostics;
- cause chain;
- diagnostic identity.

Therefore the base adapter must not invent them.

The Tadka projection should use:

```haskell
related      _ = []
diagnosticId _ = Nothing
diagnosticCause _ = Nothing
```

unless a future GHC protocol extension provides genuine relationships.

---

## 22. Fundamental Decoder API

The fundamental primitive should operate on one JSON line:

```haskell
decodeDiagnosticLine
    :: ByteString
    -> Either DecodeError GhcDiagnostic
```

This is deliberately independent of:

- file I/O;
- process management;
- streaming frameworks;
- Tadka rendering.

The library may later provide convenience functions for collections or streams, but those must be built on this primitive.

---

## 23. Stream Semantics

GHC diagnostics are naturally consumed as JSON Lines.

The implementation specification should define convenience stream behavior for:

- multiple valid lines;
- blank lines;
- CRLF line endings;
- final unterminated line;
- malformed JSON;
- valid JSON that is not an object;
- unsupported schema versions;
- a mixture of valid and invalid records.

The single-line decoder remains the normative primitive.

A collection decoder should not obscure which individual record failed.

No complex streaming abstraction is required for the initial implementation.

---

## 24. Error Algebra

Keep error layers separate.

Conceptually:

```haskell
data DecodeError = ...
data SourceBindingError = ...
data TadkaConversionError = ...
```

These represent different failure domains:

```text
JSON/protocol failure
        |
        v
DecodeError

source acquisition/binding failure
        |
        v
SourceBindingError

semantic Tadka projection failure
        |
        v
TadkaConversionError
```

Do not create a large aggregate error type unless the public API genuinely requires one.

Error constructors should preserve useful underlying information, such as:

- malformed JSON;
- unsupported schema version;
- invalid field type;
- invalid GHC coordinate;
- source unavailable;
- span conversion failure;
- empty/invalid Tadka projection where applicable.

---

## 25. Unknown Fields and Forward Compatibility

GHC's published schemas specify `additionalProperties: false`.

That matters for strict schema conformance, but `tadka-ghc` is also a consumer of an evolving external protocol.

Therefore the architecture distinguishes:

### Strict/conformance decoding

Used by schema fixtures and validation tests.

Unknown fields can be rejected.

### Production/forward-compatible decoding

May ignore unknown fields while remaining strict about:

- required fields;
- field types;
- version;
- semantic constraints.

The implementation specification must make this policy explicit rather than accidentally inheriting it from an Aeson decoder.

When production decoding ignores unknown fields, those fields should remain inspectable rather than being silently discarded outright — for example via the raw wire value or a decode-warnings channel — so that dropping a field is a visible, auditable choice rather than invisible data loss.

The library must never silently interpret a newer, unsupported schema version as an older known version.

---

## 26. Unsupported Versions

An unknown schema version must produce an explicit error.

Do not:

```text
1.3 -> pretend it is 1.2
2.0 -> pretend it is 1.x
```

The package may later add support for newer schemas without changing the conceptual architecture.

---

## 27. Tadka Projection

The projection should be a semantic mapping, not a second parser.

Conceptually:

```text
GhcDiagnostic
     |
     +-- severity ------> Tadka Severity
     +-- message -------> Tadka Doc
     +-- hints ---------> Tadka help
     +-- span ----------> Tadka Span/Context
     +-- code ----------> retained as GhcDiagnosticCode
     +-- reason --------> retained as GHC metadata
     +-- rendered ------> retained as GHC metadata
     |
     v
Tadka Diagnostic
```

The projection must not discard GHC information merely because Tadka has no corresponding field.

That is why `GhcDiagnostic` exists independently of the Tadka instance.

A second, separate projection exists for `OpaqueGhcOutput` (§4):

```text
OpaqueGhcOutput
     |
     +-- opaqueText -----> Tadka Doc (literal block)
     +-- severity -------> defaults to SevError
     |
     v
Tadka Diagnostic
```

This path is intentionally thin — there is no span, code, hints, or reason to project, because none existed in the captured output. It must remain a separate `Diagnostic` instance from the `GhcDiagnostic` one, not a fallback branch inside it, so the two are never conflated.

---

## 28. No Fourth Renderer

`tadka-ghc` must not introduce another rendering system.

It should not render:

- ANSI diagnostics;
- pretty terminal diagnostics;
- prose diagnostics;
- JSON diagnostics.

Once a diagnostic has been adapted to Tadka, Tadka's existing rendering infrastructure handles those outputs.

This preserves a single rendering architecture. Wrapping unstructured build output as `OpaqueGhcOutput` (§4) is data capture, not rendering — the literal-block presentation the user ultimately sees is still produced by Tadka's renderers, not by `tadka-ghc` itself.

---

## 29. Public Module Shape

A possible conceptual module structure is:

```text
Tadka.GHCProtocol
Tadka.GHCProtocol.Types
Tadka.GHCProtocol.Schema
Tadka.GHCProtocol.Decode
Tadka.GHCProtocol.Diagnostic
Tadka.GHCProtocol.Build
Tadka.GHCProtocol.Opaque
```

`Build` (§3) owns build-tool detection, flag injection, and subprocess capture. `Opaque` (§4) owns the `OpaqueGhcOutput` type and its minimal Tadka projection. Both are additive to the decode/diagnostic core and must not require it to change shape.

This name is deliberately distinct from `Tadka.Interop.GHC` (§5) so the two subsystems stay visibly separate rather than inviting confusion between them.

Only `Tadka.GHCProtocol` should be the supported public entry point unless there is a deliberate reason to expose additional modules.

Internal implementation modules may be reorganized without becoming compatibility commitments.

The exact module decomposition belongs in the implementation specification.

---

## 30. Totality and Robustness

The decoder should be exception-free for ordinary malformed or unsupported finite inputs within available resources.

It should not claim impossible guarantees against resource exhaustion.

Expected malformed-input cases should become typed errors rather than uncaught exceptions.

The implementation must also avoid unsafe assumptions about:

- message array length;
- hint count;
- span size;
- Unicode content;
- line endings;
- JSON object ordering;
- absent optional fields.

The build-tool wrapper (§3) and opaque capture (§4) carry the same exception-free, typed-error expectation, and must also avoid unsafe assumptions about:

- process exit codes (including no exit code, on a killed/hung process);
- interleaving of stdout and stderr;
- non-UTF-8 bytes in subprocess output;
- truncated or partial output when the process crashes mid-line;
- output produced after the process has already been reported as finished.

---

## 31. Testing Strategy

Testing should have four layers.

### 31.1 Schema fixtures

For each supported version:

- minimal valid diagnostic;
- maximal representative diagnostic;
- missing required field;
- wrong field type;
- null code;
- null span;
- optional reason;
- reason flags;
- reason category;
- optional rendered field;
- empty message array;
- empty hints;
- boundary integer values;
- unknown fields.

### 31.2 Coordinate fixtures

Cover:

- first character;
- same-line spans;
- multi-line spans;
- end-of-line;
- final character;
- empty spans;
- Unicode;
- CRLF;
- tabs;
- invalid coordinates;
- out-of-range coordinates.

### 31.3 Tadka projection tests

Verify:

- Warning -> `SevWarning`;
- Error -> `SevError`;
- message preservation;
- hint ordering;
- source/file preservation;
- multi-source compatibility;
- no fabricated Tadka code;
- no fabricated related/cause/id;
- rendered output remains inert metadata.

### 31.4 Property tests

Useful invariants include:

- if an encoder is later provided, valid decoded records re-encode consistently;
- message fragment order is preserved;
- GHC code is never converted into a Tadka code by accident;
- unsupported schema versions never silently decode;
- source binding never performs implicit file I/O;
- parser behavior is independent of Tadka rendering.

### 31.5 Build tool and opaque-output tests

Cover:

- flag injection verified for both `cabal build` and `stack build`;
- user-supplied `--ghc-options` (and equivalents) preserved alongside the injected flag;
- build-tool auto-detection, and explicit override when detection is ambiguous;
- a GHC panic/internal error with no diagnostic JSON produces `OpaqueGhcOutput`, not a decode failure that is silently swallowed;
- a build with a mix of valid diagnostic JSON lines and non-JSON lines classifies each line correctly;
- a non-zero exit with zero diagnostics still produces at least one `OpaqueGhcOutput` record referencing the exit code;
- cabal's/stack's own error or progress text is never misclassified as a GHC diagnostic;
- `OpaqueGhcOutput`'s Tadka projection is never confused with, or interchangeable with, `GhcDiagnostic`'s.

---

## 32. Architectural Invariants

The implementation must preserve these invariants:

1. GHC wire data is decoded according to its declared schema.
2. Unsupported schema versions fail explicitly.
3. GHC numeric codes remain GHC codes.
4. GHC `reason` is not conflated with severity or help.
5. GHC `rendered` is preserved but never reparsed as structured diagnostics.
6. Message fragment order is preserved.
7. Hint order and content are preserved.
8. Source acquisition is explicit.
9. Missing source is distinguishable from invalid coordinates.
10. Tadka's public APIs are used instead of `Tadka.Internal.*`.
11. Existing `Tadka.Interop.GHC` remains conceptually separate.
12. No relationships such as `related`, `cause`, or IDs are invented.
13. Parsing remains useful without Tadka rendering.
14. Tadka remains the sole rendering layer.
15. The GHC semantic representation preserves information that Tadka cannot natively represent.
16. Forward-compatible decoding policy is deliberate rather than accidental.
17. The diagnostics-as-JSON flag is injected automatically; the user is never required to pass it themselves.
18. User-supplied build-tool flags are preserved, not overridden, when the flag is injected.
19. Build output that is not valid GHC diagnostic JSON is preserved as `OpaqueGhcOutput`, never silently dropped.
20. `OpaqueGhcOutput` is never presented as, or conflated with, a faithfully decoded `GhcDiagnostic`.

---

## 33. Implementation-Spec Boundary

The next document—the implementation specification—should resolve the following concrete items.

### Types

- exact `SchemaVersion`;
- exact `GhcVersion`;
- exact `GhcSpan`;
- exact `GhcSeverity`;
- exact `GhcDiagnosticCode`;
- exact `DiagnosticReason`;
- exact `RenderedDiagnostic`;
- exact `GhcDiagnostic`.

### Wire decoding

- exact Aeson raw types;
- schema dispatch;
- validation rules;
- strict versus forward-compatible unknown-field behavior;
- numeric bounds;
- nullability;
- optional fields.

### Message construction

- exact transformation from `[Text]` to `Doc Ann`;
- preservation rules;
- handling of an empty message array.

### Hint construction

- exact transformation from `[Text]` to `help`;
- ordering and separation rules.

### Source binding

- source-provider abstraction;
- source identity;
- file lookup policy;
- missing-source behavior;
- coordinate conversion;
- stale-source behavior;
- multi-source context construction.

### Tadka adapter

- exact `Diagnostic` instance;
- exact `Context` construction;
- exact severity mapping;
- exact treatment of code, reason, rendered output;
- exact error boundaries.

### Stream handling

- line splitting;
- blank lines;
- CRLF;
- final unterminated line;
- collection APIs;
- per-record errors.

### Build tool wrapper

- cabal vs. stack detection strategy, and the override mechanism;
- exact flag-injection mechanism per build tool;
- precedence rules against user-supplied flags;
- subprocess spawning and stdout/stderr capture strategy (interleaved vs. separate streams);
- exit code handling;
- timeout/cancellation behavior.

### Opaque output

- exact `OpaqueGhcOutput` type;
- line- vs. block-grouping heuristic for multi-line panics;
- exact minimal Tadka `Diagnostic` instance for `OpaqueGhcOutput`;
- encoding/non-UTF-8 handling for raw captured text.

### Modules

- Cabal exposed modules;
- internal modules;
- dependencies;
- compatibility policy.

### Tests

- fixture corpus;
- property tests;
- malformed-input tests;
- schema-version tests;
- coordinate tests;
- Tadka integration tests;
- build-tool wrapper and opaque-output tests.

### CI

- supported GHC matrix;
- schema fixture compatibility;
- warnings-as-errors policy;
- package bounds.

---

## 34. Design Philosophy

The architecture can be summarized as:

```text
GHC protocol
     |
     | decode faithfully
     v
GhcDiagnostic
     |
     | preserve GHC-specific information
     |
     +--------------------+
     |                    |
     v                    v
GHC-domain users    explicit source binding
                          |
                          v
                    Tadka Diagnostic
                          |
                          v
                    Tadka renderers
```

The important principle is not merely that `tadka-ghc` should be small.

It is:

> **`tadka-ghc` should preserve more information than it exposes through Tadka.**

GHC's JSON protocol contains information that Tadka does not currently model directly. That information belongs in the GHC-domain representation, not in invented Tadka fields and not in a second rendering system.

Therefore:

> **Decode GHC faithfully. Preserve GHC semantics. Project into Tadka only where the semantics genuinely correspond. Let Tadka do the rendering.**

Owning the build invocation (§3) and capturing unstructured output (§4) extend this philosophy rather than compromise it: every line of build output ends up as either a faithfully decoded `GhcDiagnostic` or an honestly-labeled `OpaqueGhcOutput` — never dropped, and never disguised as the other.

That is the boundary to freeze before drafting the implementation specification.
