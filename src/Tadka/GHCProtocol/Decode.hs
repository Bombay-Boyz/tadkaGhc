-- | Wire decoding and promotion into the stable semantic type (vision
-- §22, §25; spec Phase 1.4, Phase 2.6-2.7, Phase 8).
--
-- 'DecodeError' is defined in 'Tadka.GHCProtocol.Types' (see that
-- module's Haddock for why) and re-exported here so callers importing
-- Decode.hs see it as before.
module Tadka.GHCProtocol.Decode
  ( DecodeError (..)
  , decodeSomeRawDiagnostic
  , decodeRawByVersion
  , parseFieldsV1_0
  , parseFieldsV1_1
  , parseFieldsV1_2
  , promote
  , promoteV1_0
  , promoteV1_1
  , promoteV1_2
  , DecodingMode (..)
  , DecodeWarning (..)
  , decodeDiagnosticLineWith
  , decodeDiagnosticLine
  , decodeDiagnosticStream
  ) where

import Data.Aeson (Value (..), eitherDecodeStrict)
import Data.Aeson.Types (Object, Parser, parseEither, (.:), (.:?))
import Data.Maybe (mapMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import Data.Text (Text)
import Data.List (sortOn)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM

import Tadka.GHCProtocol.Schema
import Tadka.GHCProtocol.Types

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right b) = Right b

--------------------------------------------------------------------------------
-- Per-version field parsers (spec Phase 1.4).
--------------------------------------------------------------------------------

parseFieldsV1_0 :: Object -> Either DecodeError RawFieldsV1_0
parseFieldsV1_0 obj = mapLeft (DecodeFieldTypeMismatch (mkSchemaVersion "1.0") "?" . Text.pack)
  (parseEither parser obj)
  where
    parser :: Object -> Parser RawFieldsV1_0
    parser o = RawFieldsV1_0
      <$> o .:  "ghcVersion"
      <*> o .:? "span"
      <*> o .:  "severity"
      <*> o .:? "code"
      <*> o .:  "message"
      <*> o .:  "hints"

parseFieldsV1_1 :: Object -> Either DecodeError RawFieldsV1_1
parseFieldsV1_1 obj = mapLeft (DecodeFieldTypeMismatch (mkSchemaVersion "1.1") "?" . Text.pack)
  (parseEither parser obj)
  where
    parser :: Object -> Parser RawFieldsV1_1
    parser o = RawFieldsV1_1
      <$> o .:  "ghcVersion"
      <*> o .:? "span"
      <*> o .:  "severity"
      <*> o .:? "code"
      <*> o .:  "message"
      <*> o .:  "hints"
      <*> o .:? "reason"

parseFieldsV1_2 :: Object -> Either DecodeError RawFieldsV1_2
parseFieldsV1_2 obj = mapLeft (DecodeFieldTypeMismatch (mkSchemaVersion "1.2") "?" . Text.pack)
  (parseEither parser obj)
  where
    parser :: Object -> Parser RawFieldsV1_2
    parser o = RawFieldsV1_2
      <$> o .:  "ghcVersion"
      <*> o .:? "span"
      <*> o .:  "severity"
      <*> o .:? "code"
      <*> o .:  "message"
      <*> o .:  "hints"
      <*> o .:? "reason"
      <*> o .:? "rendered"

decodeRawByVersion
  :: SKnownSchemaVersion v -> Object -> Either DecodeError (RawDiagnostic v)
decodeRawByVersion SV1_0 obj = RawDiagnosticV1_0 <$> parseFieldsV1_0 obj
decodeRawByVersion SV1_1 obj = RawDiagnosticV1_1 <$> parseFieldsV1_1 obj
decodeRawByVersion SV1_2 obj = RawDiagnosticV1_2 <$> parseFieldsV1_2 obj

--------------------------------------------------------------------------------
-- Wire-layer pipeline: JSON bytes -> version dispatch -> RawDiagnostic.
--------------------------------------------------------------------------------

expectObject :: Value -> Either DecodeError Object
expectObject (Object o) = Right o
expectObject _          = Left DecodeNotAnObject

extractSchemaVersionField :: Object -> Either DecodeError SchemaVersion
extractSchemaVersionField obj =
  case parseEither (\o -> o .: "version") obj of
    Left _  -> Left (DecodeMissingField (mkSchemaVersion "?") "version")
    Right t -> Right (mkSchemaVersion t)

decodeSomeRawDiagnostic :: ByteString -> Either DecodeError SomeRawDiagnostic
decodeSomeRawDiagnostic bs = do
  (obj, rawSv) <- parseVersionedObject bs
  case classifySchemaVersion rawSv of
    Left (UnsupportedSchemaVersion sv) -> Left (DecodeUnsupportedVersion sv)
    Right (SomeSKnownSchemaVersion sv) -> SomeRawDiagnostic sv <$> decodeRawByVersion sv obj

--------------------------------------------------------------------------------
-- Promotion into the stable semantic type (spec Phase 2.6).
--------------------------------------------------------------------------------

-- | Total per case, one clause per 'RawDiagnostic' constructor --
-- exhaustively checked because 'RawDiagnostic' is a GADT indexed by a
-- closed kind.
promote :: SomeRawDiagnostic -> Either DecodeError GhcDiagnostic
promote (SomeRawDiagnostic SV1_0 (RawDiagnosticV1_0 f)) = promoteV1_0 f
promote (SomeRawDiagnostic SV1_1 (RawDiagnosticV1_1 f)) = promoteV1_1 f
promote (SomeRawDiagnostic SV1_2 (RawDiagnosticV1_2 f)) = promoteV1_2 f

-- | ghcReason/ghcRendered are Nothing by schema 1.0's own absence of the
-- field, not by a wire-level Maybe collapsing "absent" and
-- "present-but-null" into one case -- there is no rf10Reason field to
-- read at all.
promoteV1_0 :: RawFieldsV1_0 -> Either DecodeError GhcDiagnostic
promoteV1_0 f = do
  ver   <- mkGhcVersion (rf10Version f)
  sev   <- mkGhcSeverity (rf10Severity f)
  code  <- traverse mkGhcDiagnosticCode (rf10Code f)
  span_ <- traverse promoteSpan (rf10Span f)
  pure GhcDiagnostic
    { ghcVersion  = ver
    , ghcSpan     = span_
    , ghcSeverity = sev
    , ghcCode     = code
    , ghcMessage  = rf10Message f
    , ghcHints    = rf10Hints f
    , ghcReason   = Nothing
    , ghcRendered = Nothing
    }

-- | ghcReason can be non-Nothing here (rf11Reason); ghcRendered stays
-- Nothing, since schema 1.1 has no rendered field.
promoteV1_1 :: RawFieldsV1_1 -> Either DecodeError GhcDiagnostic
promoteV1_1 f = do
  ver   <- mkGhcVersion (rf11Version f)
  sev   <- mkGhcSeverity (rf11Severity f)
  code  <- traverse mkGhcDiagnosticCode (rf11Code f)
  span_ <- traverse promoteSpan (rf11Span f)
  pure GhcDiagnostic
    { ghcVersion  = ver
    , ghcSpan     = span_
    , ghcSeverity = sev
    , ghcCode     = code
    , ghcMessage  = rf11Message f
    , ghcHints    = rf11Hints f
    , ghcReason   = promoteReason <$> rf11Reason f
    , ghcRendered = Nothing
    }

-- | Both ghcReason and ghcRendered can be non-Nothing here.
promoteV1_2 :: RawFieldsV1_2 -> Either DecodeError GhcDiagnostic
promoteV1_2 f = do
  ver   <- mkGhcVersion (rf12Version f)
  sev   <- mkGhcSeverity (rf12Severity f)
  code  <- traverse mkGhcDiagnosticCode (rf12Code f)
  span_ <- traverse promoteSpan (rf12Span f)
  pure GhcDiagnostic
    { ghcVersion  = ver
    , ghcSpan     = span_
    , ghcSeverity = sev
    , ghcCode     = code
    , ghcMessage  = rf12Message f
    , ghcHints    = rf12Hints f
    , ghcReason   = promoteReason <$> rf12Reason f
    , ghcRendered = RenderedDiagnostic <$> rf12Rendered f
    }

-- | The normative primitive (§22, §33): deliberately independent of file
-- I/O, process management, streaming frameworks, and Tadka rendering.
decodeDiagnosticLine :: ByteString -> Either DecodeError GhcDiagnostic
decodeDiagnosticLine bs = fst <$> decodeDiagnosticLineWith ForwardCompatible bs

--------------------------------------------------------------------------------
-- Stream semantics over a stream KNOWN to be GHC diagnostic JSON Lines
-- (vision §23; spec Phase 8). Distinct from Opaque.hs's classification:
-- that handles build-tool output that may contain non-diagnostic lines
-- by design; this handles a stream that is already known to be nothing
-- but diagnostics (e.g. GHC invoked directly, or a captured diagnostics
-- file).
--------------------------------------------------------------------------------

-- | Total: 'BS.take' is total for any 'Int', including a negative one,
-- so this does not rely on an adjacent guard having already proven the
-- input non-empty the way a guarded 'BS.init' would -- a function only
-- safe because of an adjacent guard is exactly the pattern this
-- codebase avoids, even when the guard happens to make it correct.
stripTrailingCR :: ByteString -> ByteString
stripTrailingCR bs
  | BS.isSuffixOf "\r" bs = BS.take (BS.length bs - 1) bs
  | otherwise              = bs

-- | Total. Blank lines are skipped (documented, not silent); CRLF is
-- normalized before decoding; every surviving line keeps its own
-- Either, so one malformed record never obscures another's success or
-- failure (§23).
decodeDiagnosticStream :: [ByteString] -> [Either DecodeError GhcDiagnostic]
decodeDiagnosticStream = mapMaybe decodeNonBlank . map stripTrailingCR
  where
    decodeNonBlank bs
      | BS.null bs = Nothing
      | otherwise  = Just (decodeDiagnosticLine bs)

--------------------------------------------------------------------------------
-- Decoding modes (vision §25; spec Phase 2.7).
--------------------------------------------------------------------------------

-- | 'Strict' rejects any top-level key outside the matched schema
-- version's recognised set (conformance testing); 'ForwardCompatible'
-- (the production default) accepts them and reports each one.
data DecodingMode = Strict | ForwardCompatible
  deriving stock (Eq, Show)

-- | A key present on the wire but not consumed by the matched schema
-- version, with its raw JSON value, so dropping it is visible and
-- auditable rather than silent data loss. Warnings are ordered by key,
-- not wire order (aeson 2.x's key map does not preserve insertion
-- order), which makes them deterministic for identical input.
data DecodeWarning = UnknownField Text Value
  deriving stock (Eq, Show)

baseFieldNames :: [Text]
baseFieldNames = ["version", "ghcVersion", "span", "severity", "code", "message", "hints"]

-- | Total: one equation per known schema version (compiler-checked).
knownFieldNames :: SKnownSchemaVersion v -> [Text]
knownFieldNames SV1_0 = baseFieldNames
knownFieldNames SV1_1 = baseFieldNames <> ["reason"]
knownFieldNames SV1_2 = baseFieldNames <> ["reason", "rendered"]

unknownFields :: SKnownSchemaVersion v -> Object -> [(Text, Value)]
unknownFields sv obj =
  sortOn fst
    [ (k, v)
    | (key, v) <- KM.toList obj
    , let k = Key.toText key
    , k `notElem` knownFieldNames sv
    ]

-- | Shared front half of every decode entry point: bytes -> top-level
-- object plus its declared schema version.
parseVersionedObject :: ByteString -> Either DecodeError (Object, SchemaVersion)
parseVersionedObject bs = do
  value <- mapLeft (DecodeMalformedJson . Text.pack)
             (eitherDecodeStrict bs :: Either String Value)
  obj   <- expectObject value
  rawSv <- extractSchemaVersionField obj
  pure (obj, rawSv)

decodeKnown :: SKnownSchemaVersion v -> Object -> Either DecodeError GhcDiagnostic
decodeKnown sv obj = decodeRawByVersion sv obj >>= promote . SomeRawDiagnostic sv

-- | Total. In 'Strict' mode the lexicographically first unknown key is
-- reported as an error; in 'ForwardCompatible' mode every unknown key
-- is returned alongside the decoded value.
decodeDiagnosticLineWith
  :: DecodingMode -> ByteString -> Either DecodeError (GhcDiagnostic, [DecodeWarning])
decodeDiagnosticLineWith mode bs = do
  (obj, rawSv) <- parseVersionedObject bs
  case classifySchemaVersion rawSv of
    Left (UnsupportedSchemaVersion sv) -> Left (DecodeUnsupportedVersion sv)
    Right (SomeSKnownSchemaVersion sv) ->
      let unknowns = unknownFields sv obj
      in case (mode, unknowns) of
           (Strict, (k, _) : _)   -> Left (DecodeUnknownField rawSv k)
           (Strict, [])           -> (\d -> (d, [])) <$> decodeKnown sv obj
           (ForwardCompatible, _) ->
             (\d -> (d, map (uncurry UnknownField) unknowns)) <$> decodeKnown sv obj
