-- | Stable GHC-domain semantic representation (vision §9-§15; spec Phase 2).
--
-- Deliberately depends only on 'Tadka.GHCProtocol.Schema' (the raw wire
-- shapes), never on Aeson: this module is the pure semantic layer, wire
-- decoding lives in 'Tadka.GHCProtocol.Decode'.
--
-- 'DecodeError' is defined here, not in Decode.hs (a deliberate deviation
-- from the module-layout sketch, noted explicitly rather than silently
-- made): this module's own smart constructors (mkGhcVersion,
-- mkGhcSeverity, mkGhcDiagnosticCode, mkLine, mkColumn, mkGhcSpan) all
-- need to *produce* DecodeError values, and Decode.hs needs GhcDiagnostic
-- from this module for 'promote' -- defining DecodeError in Decode.hs
-- would make the two modules mutually dependent. Decode.hs re-exports
-- DecodeError, so its public location for callers is unaffected.
module Tadka.GHCProtocol.Types
  ( -- * Errors
    DecodeError (..)
    -- * GhcVersion
  , GhcVersion
  , mkGhcVersion
  , unGhcVersion
    -- * GhcDiagnosticCode
  , GhcDiagnosticCode
  , mkGhcDiagnosticCode
  , unGhcDiagnosticCode
    -- * GhcSeverity
  , GhcSeverity (..)
  , mkGhcSeverity
    -- * DiagnosticReason
  , DiagnosticReason (..)
  , promoteReason
    -- * RenderedDiagnostic
  , RenderedDiagnostic (..)
    -- * Coordinates and spans (structural only; source binding is Phase 3)
  , Line
  , mkLine
  , unLine
  , Column
  , mkColumn
  , unColumn
  , GhcSpan (..)
  , mkGhcSpan
  , promoteSpan
  , renderPosition
  , renderGhcSpan
    -- * GhcDiagnostic
  , GhcDiagnostic (..)
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as Text

import Tadka.GHCProtocol.Schema
  ( RawPosition (..)
  , RawReason (..)
  , RawSpan (..)
  , SchemaVersion
  )

--------------------------------------------------------------------------------
-- Errors (spec Phase 4 later extends this same type with structured
-- JsonPath fields for DecodeMissingField/DecodeFieldTypeMismatch, I-26 --
-- edit this declaration in place then, don't introduce a second type).
--------------------------------------------------------------------------------

data DecodeError
  = DecodeMalformedJson Text
  | DecodeNotAnObject
  | DecodeMissingField SchemaVersion Text
  | DecodeFieldTypeMismatch SchemaVersion Text Text
  | DecodeUnsupportedVersion SchemaVersion
  | DecodeUnknownField SchemaVersion Text
  | InvalidGhcVersion Text
  | InvalidDiagnosticCode Integer
  | UnrecognizedSeverity Text
  | InvalidCoordinate Text Integer
  | InvalidSpanOrder (Line, Column) (Line, Column)
    -- ^ A span whose end precedes its start; carries the start and the end
    -- (in that order) exactly as they appeared on the wire.
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- GhcVersion (§9's clarifying note: distinct from SchemaVersion -- this
-- identifies the compiler, not the JSON protocol revision).
--------------------------------------------------------------------------------

newtype GhcVersion = GhcVersion Text
  deriving stock (Eq, Ord, Show)

-- | Total: rejects only the empty string. A GHC version string of any
-- other shape is accepted as-is -- this constructor does not attempt to
-- parse or validate version-number structure, only reject the
-- structurally-impossible empty case.
mkGhcVersion :: Text -> Either DecodeError GhcVersion
mkGhcVersion t
  | Text.null t = Left (InvalidGhcVersion t)
  | otherwise   = Right (GhcVersion t)

-- | The compiler version string exactly as GHC reported it (for example
-- @"9.10.1"@). Never empty: 'mkGhcVersion' is the only way to build one.
unGhcVersion :: GhcVersion -> Text
unGhcVersion (GhcVersion t) = t

--------------------------------------------------------------------------------
-- GhcDiagnosticCode (§10: preserves GHC's numeric code exactly; never
-- fabricated into a Tadka DiagnosticCode).
--------------------------------------------------------------------------------

newtype GhcDiagnosticCode = GhcDiagnosticCode Int
  deriving stock (Eq, Ord, Show)

-- | Total: rejects both ends of the range a wire Integer could fall
-- outside of 'Int'. 'Integer', not 'Int', is the wire-level type (Phase
-- 1.3's note) precisely so this function can reject out-of-range values
-- explicitly rather than the wire parser silently wrapping/truncating.
mkGhcDiagnosticCode :: Integer -> Either DecodeError GhcDiagnosticCode
mkGhcDiagnosticCode n
  | n < 0                            = Left (InvalidDiagnosticCode n)
  | n > toInteger (maxBound :: Int)  = Left (InvalidDiagnosticCode n)
  | otherwise                        = Right (GhcDiagnosticCode (fromInteger n))

-- | GHC's own numeric diagnostic code, preserved exactly (for example
-- @88464@); never converted into a Tadka diagnostic code.
unGhcDiagnosticCode :: GhcDiagnosticCode -> Int
unGhcDiagnosticCode (GhcDiagnosticCode n) = n

--------------------------------------------------------------------------------
-- GhcSeverity (§11: exactly the two values GHC's schema defines; no
-- Advice case, and an unrecognized severity fails explicitly rather than
-- defaulting).
--------------------------------------------------------------------------------

data GhcSeverity = SevWarning | SevError
  deriving stock (Eq, Show)

-- | Total: any wire string that isn't exactly "Warning" or "Error" is
-- rejected explicitly (§11's fail-fast rule for unrecognized
-- severities), consistent with §26's "never silently reinterpret".
mkGhcSeverity :: Text -> Either DecodeError GhcSeverity
mkGhcSeverity "Warning" = Right SevWarning
mkGhcSeverity "Error"   = Right SevError
mkGhcSeverity other     = Left (UnrecognizedSeverity other)

--------------------------------------------------------------------------------
-- DiagnosticReason (§14): a direct structural copy of RawReason's
-- two-shape oneOf, never a lossy simplification of it. Total: RawReason
-- is itself a closed two-constructor type, so this mapping cannot fail.
--------------------------------------------------------------------------------

data DiagnosticReason
  = ReasonFlags (NonEmpty Text)
  | ReasonCategory Text
  deriving stock (Eq, Show)

promoteReason :: RawReason -> DiagnosticReason
promoteReason (RawReasonFlags fs)    = ReasonFlags fs
promoteReason (RawReasonCategory c)  = ReasonCategory c

--------------------------------------------------------------------------------
-- RenderedDiagnostic (§15): preserved exactly as supplied by GHC, never
-- reparsed as structured diagnostics.
--------------------------------------------------------------------------------

newtype RenderedDiagnostic = RenderedDiagnostic Text
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Coordinates and GhcSpan (§16, §19's decisions; structural validation
-- only -- binding a span against real source text is Phase 3's job).
--------------------------------------------------------------------------------

newtype Line = Line Int
  deriving stock (Eq, Ord, Show)

newtype Column = Column Int
  deriving stock (Eq, Ord, Show)

-- | The one-based line number.
unLine :: Line -> Int
unLine (Line n) = n

-- | Total: rejects both ends of the range a wire Integer could fall
-- outside of. One-based, per §19/§3.1's (provisional) convention.
mkLine :: Integer -> Either DecodeError Line
mkLine n
  | n < 1                            = Left (InvalidCoordinate "line numbers are 1-based; got " n)
  | n > toInteger (maxBound :: Int)  = Left (InvalidCoordinate "line number exceeds Int range; got " n)
  | otherwise                        = Right (Line (fromInteger n))

-- | The one-based column number.
unColumn :: Column -> Int
unColumn (Column n) = n

-- | Total: same range rules as 'mkLine', for columns.
mkColumn :: Integer -> Either DecodeError Column
mkColumn n
  | n < 1                            = Left (InvalidCoordinate "columns are 1-based; got " n)
  | n > toInteger (maxBound :: Int)  = Left (InvalidCoordinate "column exceeds Int range; got " n)
  | otherwise                        = Right (Column (fromInteger n))

data GhcSpan = GhcSpan
  { spanFile      :: FilePath  -- ^ preserved verbatim, §20
  , spanStartLine :: Line
  , spanStartCol  :: Column
  , spanEndLine   :: Line
  , spanEndCol    :: Column
  } deriving stock (Eq, Show)

-- | Total: rejects a span whose end precedes its start, reporting both
-- positions in 'InvalidSpanOrder'. A zero-width span (end equal to start)
-- is accepted.
mkGhcSpan :: FilePath -> Line -> Column -> Line -> Column -> Either DecodeError GhcSpan
mkGhcSpan file sl sc el ec
  | (el, ec) < (sl, sc) = Left (InvalidSpanOrder (sl, sc) (el, ec))
  | otherwise            = Right (GhcSpan file sl sc el ec)

-- | @line:column@, for example @"5:10"@.
renderPosition :: Line -> Column -> Text
renderPosition l c = showInt (unLine l) <> ":" <> showInt (unColumn c)
  where
    showInt :: Int -> Text
    showInt = Text.pack . show

-- | A compact, stable, human-readable location: @file:line:col-line:col@,
-- for example @"Foo.hs:5:3-5:10"@. The file name is verbatim (§20). This
-- is a label for messages, not GHC's own span syntax, and the end column
-- is shown exactly as stored, with no inclusive/exclusive adjustment.
renderGhcSpan :: GhcSpan -> Text
renderGhcSpan sp =
  Text.pack (spanFile sp)
    <> ":" <> renderPosition (spanStartLine sp) (spanStartCol sp)
    <> "-" <> renderPosition (spanEndLine sp) (spanEndCol sp)

-- | Structural promotion of the wire's nested span shape into a
-- validated 'GhcSpan'. Does not touch source text -- that is Phase 3's
-- 'convertSpan'/'bindSpan', a genuinely separate operation (§17).
promoteSpan :: RawSpan -> Either DecodeError GhcSpan
promoteSpan rs = do
  sl <- mkLine   (rpLine   (rsStart rs))
  sc <- mkColumn (rpColumn (rsStart rs))
  el <- mkLine   (rpLine   (rsEnd   rs))
  ec <- mkColumn (rpColumn (rsEnd   rs))
  mkGhcSpan (Text.unpack (rsFile rs)) sl sc el ec

--------------------------------------------------------------------------------
-- GhcDiagnostic (§9's stable semantic representation).
--------------------------------------------------------------------------------

data GhcDiagnostic = GhcDiagnostic
  { ghcVersion   :: GhcVersion
  , ghcSpan      :: Maybe GhcSpan
  , ghcSeverity  :: GhcSeverity
  , ghcCode      :: Maybe GhcDiagnosticCode
  , ghcMessage   :: [Text]   -- ^ deliberately not NonEmpty: §12 forbids
                              -- rejecting an empty message array
  , ghcHints     :: [Text]   -- ^ deliberately not NonEmpty, same reasoning
  , ghcReason    :: Maybe DiagnosticReason
  , ghcRendered  :: Maybe RenderedDiagnostic
  } deriving stock (Eq, Show)
