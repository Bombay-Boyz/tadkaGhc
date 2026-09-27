# tadka-ghc

Adapter for GHC's external `-fdiagnostics-as-json` protocol, projecting
into Tadka's diagnostic model.

See `docs/tadka_ghc_vision_final.md` (architecture) and
`docs/tadka_ghc_implementation_spec_final.md` (phased build plan) for
the full design. Development proceeds phase by phase per the spec's
§11 dependency graph; do not skip ahead of a phase's own "Definition
of done" checklist.
