{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tadka projection (vision §6, §27; spec Phase 4-5), written against
-- tadka's real Diagnostic class, which gives every method except
-- 'Tadka.message' a total default (context -> NoContext, code -> Nothing,
-- severity -> SevError, help -> Nothing, url -> Nothing, related -> [],
-- diagnosticId -> Nothing, diagnosticCause -> Nothing). The spec assumed
-- every method needed an explicit definition; here, 'GhcDiagnostic'
-- overrides only 'message'/'severity'/'help', because the defaults for
-- everything else already ARE the behaviour §10/§21 require (never
-- fabricate a code, never invent related/cause/id) -- omitting them is
-- not a shortcut, it is the correct, total implementation.
--
-- Deliberately defined here rather than in 'Tadka.GHCProtocol.Types'
-- (where 'GhcDiagnostic' itself lives): this is the project's own
-- convention for where a Diagnostic bridge instance lives (mirroring
-- spec §12's "no orphan instances... defined in Diagnostic.hs" rule),
-- not a requirement of GHC's own orphan-instance rules.
module Tadka.GHCProtocol.Diagnostic
  ( -- * Message/help rendering (§12, §13)
    messageDoc
  , helpDoc
    -- * The bound variant (§5.1)
  , BoundGhcDiagnostic (..)
    -- * Reserved extension point (§24)
  , TadkaConversionError
    -- * Human-readable error rendering (§24's "render" requirement)
  , renderDecodeError
  , renderSourceBindingError
  , renderSourceLookupError
  , renderTimeoutError
  , renderBuildToolDetectionError
  ) where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as Text
import Prettyprinter (Doc, pretty, vsep)
import qualified Tadka

import Tadka.GHCProtocol.BuildTool (BuildToolDetectionError (..))
import Tadka.GHCProtocol.Process (TimeoutError (..), maxTimeoutSeconds)
import Tadka.GHCProtocol.Schema (unSchemaVersion)
import Tadka.GHCProtocol.Span
  ( CoordinateError (..)
  , SourceBindingError (..)
  , SourceLookupError (..)
  , SpanState (..)
  )
import Tadka.GHCProtocol.Types

--------------------------------------------------------------------------------
-- Message/help rendering (§12/§13's exact transformation, spec Phase 5.2).
--
-- GHC's message/hints arrays are read as ordered sequences of complete,
-- independent lines -- not word-wrapped prose fragments to be joined,
-- and not a set whose order carries no meaning (Phase 1.3's captured
-- example: a single complete sentence as one array element). 'vsep'
-- (vertical concatenation, one fragment per line, no wrapping/joining)
-- is the only choice that doesn't fabricate structure: joining with a
-- space or comma would assert an adjacency relationship the array's own
-- boundaries don't claim.
--------------------------------------------------------------------------------

-- | Total over the empty-list case, which §12 explicitly requires the
-- decoder to accept without complaint. Order preserved exactly; no
-- separator invented beyond the layout engine's own line breaks.
messageDoc :: [Text] -> Doc Tadka.Ann
messageDoc []    = mempty
messageDoc frags = vsep (map pretty frags)

-- | 'Nothing' when there are no hints, distinguishing "no hints" from
-- "hints present" at the type level rather than rendering an empty
-- bulleted list (§13: nothing is discarded, there is simply nothing to
-- render). Order preserved exactly, same fragment semantics as
-- 'messageDoc'.
helpDoc :: [Text] -> Maybe (Doc Tadka.Ann)
helpDoc []    = Nothing
helpDoc hints = Just (vsep (map (("\8226 " <>) . pretty) hints))

--------------------------------------------------------------------------------
-- GhcDiagnostic's Tadka.Diagnostic instance (§5.1).
--------------------------------------------------------------------------------

instance Tadka.Diagnostic GhcDiagnostic where
  message d = messageDoc (ghcMessage d)

  -- Total, exactly the two-case mapping §11 specifies. No Advice branch
  -- exists to write, because GhcSeverity itself has only two
  -- constructors.
  severity d = case ghcSeverity d of
    SevWarning -> Tadka.SevWarning
    SevError   -> Tadka.SevError

  help d = helpDoc (ghcHints d)

  -- context, code, related, diagnosticId, diagnosticCause, url: all
  -- deliberately omitted here, relying on Tadka.Diagnostic's own class
  -- defaults (NoContext / Nothing / [] / Nothing / Nothing / Nothing).
  -- This is not an oversight:
  --   * context: no source binding has been attempted against a plain
  --     GhcDiagnostic -- NoContext is honestly correct here. A caller
  --     who has bound source uses BoundGhcDiagnostic below instead.
  --   * code: §10's base-adapter rule -- never fabricate a Tadka
  --     DiagnosticCode from GHC's numeric one. The original stays
  --     reachable via ghcCode on the GhcDiagnostic value itself.
  --   * related/diagnosticId/diagnosticCause: §21 -- GHC's JSON schemas
  --     provide none of these, so none are invented.
  --   * url: GHC's own JSON diagnostics carry no associated URL.

--------------------------------------------------------------------------------
-- BoundGhcDiagnostic: a GhcDiagnostic paired with the outcome of binding
-- it against source material (§5.1, Phase 3's SpanState).
--------------------------------------------------------------------------------

data BoundGhcDiagnostic = BoundGhcDiagnostic
  { boundDiagnostic :: GhcDiagnostic
  , boundSpanState  :: SpanState
  } deriving stock (Show)

instance Tadka.Diagnostic BoundGhcDiagnostic where
  message b  = Tadka.message (boundDiagnostic b)
  severity b = Tadka.severity (boundDiagnostic b)
  help b     = Tadka.help (boundDiagnostic b)

  -- Total, one equation per SpanState constructor (compiler-checked
  -- exhaustiveness -- SpanState gaining another constructor later, e.g.
  -- a future SpanStale, is a compile error here until this case is
  -- updated, not a silent NoContext by omission).
  context b = case boundSpanState b of
    SpanBound ctx               -> ctx
    NoSpan                      -> Tadka.NoContext
    SpanNoSource _               -> Tadka.NoContext
    SpanSourceUnavailable _ _    -> Tadka.NoContext
    SpanInvalidCoordinates _ _   -> Tadka.NoContext

  -- code/related/diagnosticId/diagnosticCause/url: rely on the class
  -- defaults, identical reasoning to GhcDiagnostic's instance above.

--------------------------------------------------------------------------------
-- Reserved extension point (§24): currently uninhabited, since this
-- base adapter's projection is provably total -- no constructor exists
-- to be thrown. An empty type is a stronger statement of "this cannot
-- fail" than an Either whose Left is never constructed.
--------------------------------------------------------------------------------

data TadkaConversionError

--------------------------------------------------------------------------------
-- Human-readable error rendering (§24: never Show-derived text shown to
-- end users -- Show output is for debugging, not for a rendered
-- diagnostic).
--------------------------------------------------------------------------------

renderDecodeError :: DecodeError -> Text
renderDecodeError e = case e of
  DecodeMalformedJson msg ->
    "malformed JSON: " <> msg
  DecodeNotAnObject ->
    "expected a JSON object at the top level"
  DecodeMissingField sv field ->
    "schema " <> unSchemaVersion sv <> ": missing required field \"" <> field <> "\""
  DecodeFieldTypeMismatch sv field expected ->
    "schema " <> unSchemaVersion sv <> ": field \"" <> field
      <> "\" has the wrong type (expected " <> expected <> ")"
  DecodeUnsupportedVersion sv ->
    "unsupported diagnostics-as-json schema version: " <> unSchemaVersion sv
  DecodeUnknownField sv field ->
    "schema " <> unSchemaVersion sv <> ": unknown field \"" <> field <> "\" (rejected in strict mode)"
  InvalidGhcVersion t ->
    "invalid GHC version string: \"" <> t <> "\""
  InvalidDiagnosticCode n ->
    "invalid GHC diagnostic code: " <> Text.pack (show n)
  UnrecognizedSeverity t ->
    "unrecognized GHC severity: \"" <> t <> "\" (expected \"Warning\" or \"Error\")"
  InvalidCoordinate reason n ->
    reason <> Text.pack (show n)
  InvalidSpanOrder (sl, sc) (el, ec) ->
    "span ends at " <> renderPosition el ec
      <> " before it starts at " <> renderPosition sl sc

renderSourceBindingError :: SourceBindingError -> Text
renderSourceBindingError e = case e of
  InvalidCoordinates sp reason ->
    "invalid coordinates in " <> renderGhcSpan sp <> ": " <> describeCoordinateError reason
  TadkaSpanRejected sp err ->
    "tadka rejected the span for " <> renderGhcSpan sp <> ": " <> describeSpanBuildError err
  TadkaSourceRejected sp err ->
    "tadka rejected the source name for " <> renderGhcSpan sp <> ": " <> describeSourceError err
  TadkaContextRejected sp err ->
    "span out of bounds against real source for " <> renderGhcSpan sp <> ": " <> describeContextError err

describeCoordinateError :: CoordinateError -> Text
describeCoordinateError e = case e of
  LineOutOfRange l ->
    "line " <> showInt (unLine l) <> " is beyond the end of the source"
  ColumnOutOfRange l c ->
    "column " <> showInt (unColumn c) <> " is beyond the end of line " <> showInt (unLine l)
  ColumnInsideTab l c ->
    "column " <> showInt (unColumn c) <> " of line " <> showInt (unLine l)
      <> " falls inside a tab character (a tab advances to the next multiple-of-8 column), so the source may differ from what GHC compiled"
  where
    showInt :: Int -> Text
    showInt = Text.pack . show

-- tadka's error types derive only 'Show' and expose no public renderer, so
-- each constructor gets a fixed, documented sentence here instead of
-- interpolating 'show' output (which would print record syntax at the
-- user). The payload of 'Tadka.SpanBadOffset' / 'Tadka.SpanBadLength' is
-- not exported by tadka and carries nothing beyond which field was
-- negative, so it is matched with a wildcard. These matches are
-- exhaustive against tadka 2.0.x (the bound in the cabal file); a new
-- constructor in a future tadka raises -Wincomplete-patterns here (an error
-- under the werror flag and in CI), which is the intended prompt to update
-- this module.

describeSpanBuildError :: Tadka.SpanBuildError -> Text
describeSpanBuildError (Tadka.SpanBadOffset _) = "the start offset is negative"
describeSpanBuildError (Tadka.SpanBadLength _) = "the length is negative"

describeSourceError :: Tadka.SourceError -> Text
describeSourceError Tadka.EmptySourceName = "the source name is empty"

describeContextError :: Tadka.ContextError -> Text
describeContextError (Tadka.ContextError (Tadka.SpanOutOfBoundsError spanEnd sourceChars)) =
  "the span ends at character " <> Text.pack (show spanEnd)
    <> " but the source has only " <> Text.pack (show sourceChars) <> " characters"

-- | Kept for completeness alongside the two renderers above, though not
-- currently called from anywhere in this module.
renderSourceLookupError :: SourceLookupError -> Text
renderSourceLookupError e = case e of
  SourceIOError msg         -> "source lookup failed: " <> msg
  SourceInvalidEncoding msg -> "source is not valid text: " <> msg

-- | Why a timeout value was rejected, as a phrase that completes
-- "timeout ...", for example "timeout must be greater than 0".
renderTimeoutError :: TimeoutError -> Text
renderTimeoutError e = case e of
  TimeoutNotFinite   -> "must be a finite number"
  TimeoutNotPositive -> "must be greater than 0"
  TimeoutTooLarge    ->
    "must be at most " <> Text.pack (show (truncate maxTimeoutSeconds :: Integer))
      <> " seconds (one year)"

renderBuildToolDetectionError :: BuildToolDetectionError -> Text
renderBuildToolDetectionError e = case e of
  NoRecognizedProjectFile ->
    "no stack.yaml, .cabal file or cabal.project found in the project directory"
  AmbiguousProjectFiles files ->
    "more than one .cabal file found (" <> Text.intercalate ", " (map Text.pack (toList files))
      <> ") and no stack.yaml; the build tool cannot be chosen automatically"
  ProjectDirectoryUnreadable _dir reason ->
    "the project directory cannot be read: " <> reason
