-- | Coordinate conversion and source/span binding (vision §16-§20; spec
-- Phase 3), written against tadka's real public API (tadka-2.0.0.0 from
-- Hackage) rather than the vision/spec's assumed signatures. Two
-- concrete deltas from what the spec assumed, resolved here:
--
--   * 'Tadka.mkNamedSource' and 'Tadka.mkContext' both return 'Either'
--     (the spec assumed bare values for both).
--   * 'Tadka.mkContext' bounds-checks the span against the real source
--     length internally (via its own 'resolveSpan') -- so tadka-ghc does
--     NOT need to duplicate an end-of-source bounds check itself; a span
--     whose coordinates lie outside the source surfaces as a
--     'TadkaContextRejected', not as a separate check in this module.
module Tadka.GHCProtocol.Span
  ( -- * Source text and lookup
    SourceText (..)
  , SourceProvider (..)
  , SourceLookupError (..)
  , fileSourceProvider
    -- * Span state (§18's table, exactly the states it enumerates)
  , SpanState (..)
  , SourceBindingError (..)
  , CoordinateError (..)
    -- * Coordinate conversion
  , LineMetadata (..)
  , computeLineMetadata
  , coordinateToOffset
    -- * Binding
  , convertSpan
  , bindSpan
  ) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8')
import System.IO.Error (isDoesNotExistError)

import qualified Tadka

import Tadka.GHCProtocol.Types

--------------------------------------------------------------------------------
-- Source text and lookup (§17: "must not silently read arbitrary files").
--------------------------------------------------------------------------------

newtype SourceText = SourceText Text
  deriving stock (Eq, Show)

-- | A source-lookup attempt can fail in ways that are not "the file
-- doesn't exist": permission denied, a transient IO failure, or bytes
-- that aren't valid text. Each is data, never an exception escaping
-- 'lookupSource'.
data SourceLookupError
  = SourceIOError Text
  | SourceInvalidEncoding Text
  deriving stock (Eq, Show)

-- | Explicit, caller-supplied capability: tadka-ghc never reaches for
-- the filesystem on its own. 'Left' is a genuine lookup failure;
-- 'Right Nothing' is "looked up cleanly, no such source" (not an error
-- -- §2's "must remain useful even when no source text is available");
-- 'Right (Just _)' is success.
newtype SourceProvider m = SourceProvider
  { lookupSource :: FilePath -> m (Either SourceLookupError (Maybe SourceText)) }

-- | Opt-in convenience: never invoked implicitly by any decode/promotion
-- function, only by a caller who explicitly passes it to 'bindSpan'.
-- Catches only the specific IO exceptions that correspond to a missing
-- file; any other exception propagates rather than being silently
-- absorbed.
fileSourceProvider :: SourceProvider IO
fileSourceProvider = SourceProvider $ \path -> do
  result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  case result of
    Left e
      | isDoesNotExistError e -> pure (Right Nothing)
      | otherwise              -> pure (Left (SourceIOError (Text.pack (show e))))
    Right bytes -> case decodeUtf8' bytes of
      Left ue -> pure (Left (SourceInvalidEncoding (Text.pack (show ue))))
      Right t -> pure (Right (Just (SourceText t)))

--------------------------------------------------------------------------------
-- Source-binding errors, reflecting tadka's real (Either-returning) API.
--------------------------------------------------------------------------------

-- | Every way binding a 'GhcSpan' against real source text can fail.
-- The first constructor is tadka-ghc's own coordinate-conversion
-- failure (line/column doesn't fit the source found); the other three
-- wrap tadka's own three validation points in the pipeline
-- (mkSpan/mkNamedSource/mkContext) -- each genuinely reachable, not
-- merely present for totality's sake: 'TadkaContextRejected' in
-- particular is how an out-of-source-bounds span is actually reported,
-- since tadka's own 'mkContext' does that bounds check, not this module.
data SourceBindingError
  = InvalidCoordinates GhcSpan CoordinateError
  | TadkaSpanRejected GhcSpan Tadka.SpanBuildError
  | TadkaSourceRejected GhcSpan Tadka.SourceError
  | TadkaContextRejected GhcSpan Tadka.ContextError
  deriving stock (Show)

-- | Why a (line, column) pair could not be turned into a character
-- offset in the source that was found. Typed, so a renderer states the
-- real reason instead of a fixed phrase (audit R3.3).
data CoordinateError
  = LineOutOfRange Line
    -- ^ The source has fewer lines than the span's line number.
  | ColumnOutOfRange Line Column
    -- ^ The column lies beyond the end of its line.
  | ColumnInsideTab Line Column
    -- ^ The column falls strictly inside a tab character's expansion, so
    -- it names no character. GHC never reports such a start; seeing one
    -- means the source differs from what GHC compiled.
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Span state (§18's table, exactly the states it enumerates -- the
-- deferred "span becomes invalid against changed source" row has no
-- constructor here, per §18's own amendment).
--------------------------------------------------------------------------------

data SpanState
  = NoSpan
  | SpanNoSource GhcSpan
  | SpanSourceUnavailable GhcSpan SourceLookupError
  | SpanInvalidCoordinates GhcSpan SourceBindingError
  | SpanBound Tadka.Context
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Coordinate conversion (§19's decisions, as implemented in Phase 2's
-- GhcSpan/Line/Column; this is the layer that turns them into a
-- character offset).
--------------------------------------------------------------------------------

-- | Per-line metadata: the character offset (0-based) a line starts at,
-- and its text -- deliberately excluding a trailing '\r' on a CRLF
-- source (§3.1's CRLF decision), so the CR is never reachable as a
-- counted column. The text is kept because converting a GHC column to
-- an offset needs the characters before it (a tab is not one column).
data LineMetadata = LineMetadata
  { lineStartOffset :: Int
  , lineContent     :: Text
  } deriving stock (Eq, Show)

-- | Total: '\n' is the sole line separator for counting (§3.1); one
-- linear left-to-right pass over the split segments, each visited
-- exactly once, so cost is proportional to line count, not source
-- length squared. A source ending in a trailing '\n' correctly produces
-- a final, empty segment -- an empty final line, not an absent one.
computeLineMetadata :: Text -> NonEmpty LineMetadata
computeLineMetadata src = go 0 (Text.splitOn "\n" src)
  where
    go :: Int -> [Text] -> NonEmpty LineMetadata
    go !offset [l]      = LineMetadata offset (withoutCR l) :| []
    go !offset (l : ls) =
      let meta     = LineMetadata offset (withoutCR l)
          consumed = Text.length l + 1  -- +1 for the '\n' just split on
      in meta <| go (offset + consumed) ls
    go !offset []       =
      LineMetadata offset "" :| []  -- unreachable: splitOn never returns []

    withoutCR :: Text -> Text
    withoutCR l = fromMaybe l (Text.stripSuffix "\r" l)

-- | Total: converts a 1-based (Line, Column) pair, as GHC reports it,
-- into a 0-based character offset.
--
-- GHC's column rules, confirmed against real GHC 9.14.1 output (see
-- test/fixtures/coordinate/tab-real-*):
--
--   * columns count Unicode characters, not bytes;
--   * a tab advances the column to the next multiple of 8, plus 1
--     (GHC's @advanceSrcLoc@ tab-stop rule), so a tab at column 1 is
--     followed by a character at column 9;
--   * the end of a span is exclusive: one position past its last
--     character (§3.1), so the column just past the end of a line's
--     content is legal.
coordinateToOffset :: NonEmpty LineMetadata -> Line -> Column -> Either CoordinateError Int
coordinateToOffset lineMeta line col =
  case drop (unLine line - 1) (toList lineMeta) of
    []                               -> Left (LineOutOfRange line)
    (LineMetadata start content : _) ->
      case columnToIndex content (unColumn col) of
        Right i            -> Right (start + i)
        Left BeyondLine    -> Left (ColumnOutOfRange line col)
        Left InsideTabStop -> Left (ColumnInsideTab line col)

data ColumnProblem = BeyondLine | InsideTabStop

-- | The 0-based index of the character that starts at the given 1-based
-- GHC column, or one past the last character if the column is exactly
-- the exclusive end of the line. Total: each step raises the running
-- column by at least 1, so the walk ends when the column reaches or
-- passes the target or the line runs out.
columnToIndex :: Text -> Int -> Either ColumnProblem Int
columnToIndex content target = go 0 1 (Text.unpack content)
  where
    go :: Int -> Int -> String -> Either ColumnProblem Int
    go !i !c cs
      | c == target = Right i
      | c >  target = Left InsideTabStop  -- only a tab can skip columns
      | otherwise   = case cs of
          []        -> Left BeyondLine
          ch : rest -> go (i + 1) (advance ch c) rest

    advance :: Char -> Int -> Int
    advance '\t' c = ((c - 1) `div` tabStop + 1) * tabStop + 1
    advance _    c = c + 1

    tabStop :: Int
    tabStop = 8

--------------------------------------------------------------------------------
-- Binding: GhcSpan + real source text -> a real Tadka.Context.
--------------------------------------------------------------------------------

-- | Pure. Implements §3.1's conventions and constructs a real
-- 'Tadka.Context' via tadka's actual public pipeline: character offsets
-- via 'Tadka.mkSpan', wrapped into a named source via
-- 'Tadka.mkNamedSource', attached as a single primary label via
-- 'Tadka.Labeled', assembled via 'Tadka.mkContext'. No label text is
-- attached beyond marking the span itself as primary -- GHC's own
-- diagnostic carries no separate annotation string for this location,
-- so none is fabricated (§1's "never distort data" principle).
--
-- Note what this function does NOT do: it does not itself check that
-- the end offset lies within the source. That check is tadka's own
-- 'Tadka.mkContext' (via its internal 'resolveSpan') -- surfaced here as
-- 'TadkaContextRejected'. Duplicating it here would be exactly the kind
-- of "duplicate Tadka's own abstraction" §19 warns against.
convertSpan :: SourceText -> GhcSpan -> Either SourceBindingError Tadka.Context
convertSpan (SourceText src) sp = do
  startOff <- first (InvalidCoordinates sp) (coordinateToOffset lineMeta (spanStartLine sp) (spanStartCol sp))
  endOff   <- first (InvalidCoordinates sp) (coordinateToOffset lineMeta (spanEndLine sp)   (spanEndCol sp))
  spanVal  <- first (TadkaSpanRejected sp) (Tadka.mkSpan startOff (endOff - startOff))
  named    <- first (TadkaSourceRejected sp) (Tadka.mkNamedSource (Text.pack (spanFile sp)) src)
  let labeled = Tadka.Labeled spanVal Tadka.Primary Nothing :| []
  first (TadkaContextRejected sp) (Tadka.mkContext named labeled)
  where
    lineMeta = computeLineMetadata src

-- | Total for every combination of 'Maybe GhcSpan' and every possible
-- 'SourceProvider' result, including a genuine lookup failure.
bindSpan :: Monad m => SourceProvider m -> Maybe GhcSpan -> m SpanState
bindSpan _        Nothing    = pure NoSpan
bindSpan provider (Just spn) = do
  found <- lookupSource provider (spanFile spn)
  pure $ case found of
    Left err         -> SpanSourceUnavailable spn err
    Right Nothing    -> SpanNoSource spn
    Right (Just src) -> case convertSpan src spn of
      Left err  -> SpanInvalidCoordinates spn err
      Right ctx -> SpanBound ctx
