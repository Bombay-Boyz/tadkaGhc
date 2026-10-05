-- | The supported public entry point (spec §29/Phase 2's module layout;
-- vision: only this module is the sanctioned import).
--
-- The export list is curated and explicit on purpose. It contains what a
-- consumer needs to decode GHC's @-fdiagnostics-as-json@ output, bind
-- spans to source, project into Tadka, and classify a build's output.
-- Wire-level plumbing (per-schema parsers and promotions), the
-- coordinate-conversion internals, the chunk framer, the per-stream
-- classifier state machine and the flag-token helpers stay in their own
-- modules, which are implementation details without a compatibility
-- commitment (spec §29).
--
-- Deliberately does NOT re-export 'Tadka.GHCProtocol.Runner': that lives
-- in the separate 'tadka-ghc-process' component (I-35).
module Tadka.GHCProtocol
  ( -- * Diagnostics
    GhcDiagnostic (..)
  , GhcVersion
  , mkGhcVersion
  , unGhcVersion
  , GhcDiagnosticCode
  , mkGhcDiagnosticCode
  , unGhcDiagnosticCode
  , GhcSeverity (..)
  , mkGhcSeverity
  , DiagnosticReason (..)
  , RenderedDiagnostic (..)

    -- * Spans and coordinates
  , GhcSpan (..)
  , mkGhcSpan
  , renderGhcSpan
  , Line
  , mkLine
  , unLine
  , Column
  , mkColumn
  , unColumn

    -- * Decoding
  , DecodeError (..)
  , DecodingMode (..)
  , DecodeWarning (..)
  , decodeDiagnosticLine
  , decodeDiagnosticLineWith
  , decodeDiagnosticStream

    -- * Binding spans to source
  , SourceText (..)
  , SourceProvider (..)
  , SourceLookupError (..)
  , fileSourceProvider
  , SpanState (..)
  , SourceBindingError (..)
  , CoordinateError (..)
  , convertSpan
  , bindSpan

    -- * Projection into Tadka
  , BoundGhcDiagnostic (..)
  , messageDoc
  , helpDoc
  , TadkaConversionError

    -- * Human-readable error rendering
  , renderDecodeError
  , renderSourceBindingError
  , renderSourceLookupError
  , renderTimeoutError
  , renderBuildToolDetectionError

    -- * Build tools
  , BuildTool (..)
  , BuildToolDetectionError (..)
  , detectBuildTool
  , injectDiagnosticsFlag

    -- * Build results and output classification
  , OutputStream (..)
  , CompilerResult (..)
  , Signal (..)
  , ProcessError (..)
  , BuildResult (..)
  , Timeout
  , TimeoutError (..)
  , mkTimeout
  , timeoutMicroseconds
  , maxTimeoutSeconds
  , OpaqueGhcOutput (..)
  , LineClassification (..)
  , BuildOutcome (..)
  , classifyBuildOutput
  , attachCompilerResult
  ) where

import Tadka.GHCProtocol.BuildTool
import Tadka.GHCProtocol.Decode
import Tadka.GHCProtocol.Diagnostic
import Tadka.GHCProtocol.Opaque
import Tadka.GHCProtocol.Process
import Tadka.GHCProtocol.Span
import Tadka.GHCProtocol.Types
