-- | Unstructured / opaque build-output capture (vision §4; spec Phase 7).
--
-- OpaqueGhcOutput's Tadka.Diagnostic instance is defined HERE, not in
-- Diagnostic.hs as the module-layout sketch suggested: OpaqueGhcOutput
-- is defined in this module, so the instance is non-orphan by GHC's own
-- rule (no -Wno-orphans needed), and it needs nothing from
-- Diagnostic.hs's messageDoc/helpDoc machinery -- it is deliberately a
-- thin, independent instance (§27's "second, separate projection").
module Tadka.GHCProtocol.Opaque
  ( -- * The opaque record
    OpaqueGhcOutput (..)
    -- * Classification
  , LineClassification (..)
  , ClassifierState (..)
  , isPanicMarker
    -- * Per-stream and interleaved classification
  , classifyStream
  , InterleavedState (..)
  , classifyInterleaved
    -- * Whole-build classification
  , BuildOutcome (..)
  , classifyBuildOutput
  , opaqueMessage
  , attachCompilerResult
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Prettyprinter (pretty)
import qualified Tadka

import Tadka.GHCProtocol.Decode (decodeDiagnosticLine)
import Tadka.GHCProtocol.Process
  ( BuildResult (..)
  , CompilerResult (..)
  , OutputStream (..)
  , renderCompilerResult
  )
import Tadka.GHCProtocol.Types (GhcDiagnostic)

--------------------------------------------------------------------------------
-- The opaque record (§4, refined from the vision's single-field sketch
-- into a payload/rendering split, spec Phase 7.1).
--------------------------------------------------------------------------------

data OpaqueGhcOutput = OpaqueGhcOutput
  { opaqueSource         :: OutputStream
  , opaqueRawBytes       :: ByteString
    -- ^ Captured content exactly as received -- never decoded, never
    -- modified. Payload-only: the '\n' that terminated each line is
    -- framing metadata (consumed by BuildTool.hs's framer), not
    -- diagnostic content, and is not present here.
  , opaqueText           :: Text
    -- ^ A total, lenient rendering of 'opaqueRawBytes' for display --
    -- substitutes U+FFFD for invalid UTF-8, so it is not guaranteed to
    -- be a byte-for-byte-faithful view of 'opaqueRawBytes'.
  , opaqueCompilerResult :: Maybe CompilerResult
    -- ^ An explicit projection of the whole build's outcome (Phase 6),
    -- attached as a separate post-processing pass (§7.4) -- never the
    -- primary representation of success/failure. See 'BuildOutcome'.
  } deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Classification result.
--------------------------------------------------------------------------------

data LineClassification
  = ClassifiedDiagnostic GhcDiagnostic
  | ClassifiedOpaque OpaqueGhcOutput
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Panic-block grouping: an explicit, total finite state machine
-- (spec Phase 7.3).
--------------------------------------------------------------------------------

data ClassifierState
  = Idle
  | AccumulatingPanic (NonEmpty ByteString)
  deriving stock (Eq, Show)

-- | Recognizes the start of a GHC panic banner by prefix, not exact-line
-- equality: GHC appends a version and summary on the same line after
-- "ghc: panic!", which this deliberately does not require to match.
-- The literal prefix itself is an assumption pending confirmation
-- against real captured `ghc: panic!` output per GHC family (I-14,
-- external verification required, not yet performed here).
isPanicMarker :: ByteString -> Bool
isPanicMarker = ("ghc: panic!" `BS.isPrefixOf`)

isBlank :: ByteString -> Bool
isBlank = BS.null . BSC.dropWhileEnd isSpace' . BSC.dropWhile isSpace'
  where
    isSpace' c = c == ' ' || c == '\t' || c == '\r'

-- | Total: every (state, input line) pair is handled by exactly one of
-- four equations, mirroring ClassifierState's two constructors crossed
-- with the two conditions checked in each. The blank line that
-- terminates a panic block is folded into the block itself, so every
-- input line ends up inside exactly one LineClassification.
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

-- | Total: handles a panic block that runs to end-of-input with no
-- trailing blank line, so nothing accumulated is ever lost.
finalize :: OutputStream -> ClassifierState -> [LineClassification]
finalize _      Idle                    = []
finalize stream (AccumulatingPanic acc) = [ClassifiedOpaque (mkOpaqueBlock stream acc)]

-- | Total by structural recursion on a finite list. Classifies one
-- already-separated stream in isolation.
classifyStream :: OutputStream -> [ByteString] -> [LineClassification]
classifyStream stream = go Idle
  where
    go st []       = finalize stream st
    go st (l : ls) = let (st', out) = step stream st l in out <> go st' ls

--------------------------------------------------------------------------------
-- Two-stream interleaving (I-04, I-13).
--------------------------------------------------------------------------------

-- | One 'ClassifierState' per 'OutputStream', threaded independently
-- through the single interleaved list a build actually produces -- a
-- panic pending on one stream must not be disturbed by an unrelated
-- line arriving on the other.
data InterleavedState = InterleavedState
  { stdOutState :: ClassifierState
  , stdErrState :: ClassifierState
  } deriving stock (Eq, Show)

initialInterleavedState :: InterleavedState
initialInterleavedState = InterleavedState Idle Idle

-- | Total by structural recursion. A completed record is emitted
-- immediately at the position corresponding to its completing input
-- line -- this is what gives the result its "observed order" guarantee
-- (I-19), never a claim about the child's true cross-stream write
-- order. At end-of-input, both streams' pending states are flushed in a
-- fixed order (stdout, then stderr).
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

--------------------------------------------------------------------------------
-- Whole-build classification and compiler-result attachment (spec
-- Phase 7.4).
--------------------------------------------------------------------------------

data BuildOutcome = BuildOutcome
  { buildCompilerResult  :: CompilerResult
    -- ^ Never inferred from whether any line happened to decode
    -- successfully -- a build that failed with exit code 1 but whose
    -- output was all valid diagnostics still reports the failure here.
  , buildClassifications :: [LineClassification]
  } deriving stock (Show)

classifyBuildOutput :: BuildResult -> BuildOutcome
classifyBuildOutput (BuildResult result ls) =
  BuildOutcome result (attachCompilerResult result (classifyInterleaved ls))

-- | Total: attaches the build's final CompilerResult to every
-- OpaqueGhcOutput already produced. If classification produced no
-- records at all yet the build did not succeed, exactly one synthetic
-- record is introduced -- the only case a record is manufactured rather
-- than derived from captured output -- so the failure is never silently
-- lost purely at the LineClassification level.
attachCompilerResult :: CompilerResult -> [LineClassification] -> [LineClassification]
attachCompilerResult CompilerSucceeded cs = cs
attachCompilerResult result            [] =
  [ClassifiedOpaque (OpaqueGhcOutput StdErr BS.empty Text.empty (Just result))]
attachCompilerResult result            cs = map attach cs
  where
    attach (ClassifiedOpaque o)       = ClassifiedOpaque o { opaqueCompilerResult = Just result }
    attach d@(ClassifiedDiagnostic _) = d

--------------------------------------------------------------------------------
-- OpaqueGhcOutput's Tadka.Diagnostic instance (§5.3, §27): thin by
-- design -- no span, code, hints, or reason to project, because none of
-- that genuinely existed in the captured output. Only 'message' and
-- 'severity' are overridden; everything else relies on the class's own
-- defaults (help/code/related/diagnosticId/diagnosticCause/url ->
-- Nothing/[]/Nothing; context -> NoContext), same reasoning as
-- GhcDiagnostic's instance.
--------------------------------------------------------------------------------

instance Tadka.Diagnostic OpaqueGhcOutput where
  severity _ = Tadka.SevError
  message o  = pretty (opaqueMessage o)

-- | The text shown for a record: the captured text itself, except for the
-- synthetic record introduced when a build failed with no output at all.
-- That record has no text, and rendering it as-is would print a bare
-- "error:", losing the failure; so there the real cause (exit status,
-- signal, timeout, or why the build tool could not be started) is shown.
opaqueMessage :: OpaqueGhcOutput -> Text
opaqueMessage o
  | Text.null (Text.strip (opaqueText o)) =
      fromMaybe (opaqueText o) (opaqueCompilerResult o >>= renderCompilerResult)
  | otherwise = opaqueText o
