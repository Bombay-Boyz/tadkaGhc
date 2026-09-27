-- | Wire decoding (vision §22, §25; spec Phase 1.4, Phase 2.7, Phase 8).
--
-- NOTE: 'DecodeError' here carries only the constructors Phase 1 needs to
-- compile. Phase 4 (Error Algebra) extends this same type with the full
-- constructor set (DecodeMissingField/DecodeFieldTypeMismatch/etc. with
-- structured JsonPath, per I-26) -- edit this file in place then, don't
-- create a second DecodeError elsewhere.
module Tadka.GHCProtocol.Decode
  ( DecodeError (..)
  , decodeRawByVersion
  , parseFieldsV1_0
  , parseFieldsV1_1
  , parseFieldsV1_2
  ) where

import Data.Aeson (Object)
import Data.Text (Text)

import Tadka.GHCProtocol.Schema

-- | Placeholder for Phase 1; superseded (same type, more constructors,
-- same module) by Phase 4.
data DecodeError
  = DecodeMalformedJson Text
  | DecodeNotAnObject
  | DecodeMissingField SchemaVersion Text
  | DecodeFieldTypeMismatch SchemaVersion Text Text
  | DecodeUnsupportedVersion SchemaVersion
  deriving stock (Eq, Show)

-- | Total per case: exactly one Aeson object-parser per known version,
-- selected by the exhaustively-checked witness (spec Phase 1.4).
decodeRawByVersion
  :: SKnownSchemaVersion v -> Object -> Either DecodeError (RawDiagnostic v)
decodeRawByVersion SV1_0 obj = RawDiagnosticV1_0 <$> parseFieldsV1_0 obj
decodeRawByVersion SV1_1 obj = RawDiagnosticV1_1 <$> parseFieldsV1_1 obj
decodeRawByVersion SV1_2 obj = RawDiagnosticV1_2 <$> parseFieldsV1_2 obj

-- TODO(Phase 1, continued): implement these against Aeson's Object using
-- (.:)/(.:?) converted to Either via Data.Aeson.Types.parseEither, per
-- the confirmed field set in Schema.hs's Haddock (Phase 1.3). Left as
-- stubs here so the module graph and GADT dispatch compile end-to-end
-- before the parsing logic itself is filled in.
parseFieldsV1_0 :: Object -> Either DecodeError RawFieldsV1_0
parseFieldsV1_0 = error "TODO Phase 1: parseFieldsV1_0"

parseFieldsV1_1 :: Object -> Either DecodeError RawFieldsV1_1
parseFieldsV1_1 = error "TODO Phase 1: parseFieldsV1_1"

parseFieldsV1_2 :: Object -> Either DecodeError RawFieldsV1_2
parseFieldsV1_2 = error "TODO Phase 1: parseFieldsV1_2"
