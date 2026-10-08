{-# OPTIONS_HADDOCK hide #-}
-- | Schema-version and per-version wire types (vision §7, §8; spec Phase 1).
--
-- The only GADT family in this codebase (spec Phase 1, top note): a
-- type-level invariant ("this dispatch is exhaustively checked against a
-- known schema tag") that would otherwise need an unsafe fallback branch.
module Tadka.GHCProtocol.Schema
  ( -- * Public, open schema version
    SchemaVersion
  , mkSchemaVersion
  , unSchemaVersion
    -- * Closed, known-version universe
  , SchemaVersionTag (..)
  , SKnownSchemaVersion (..)
  , SomeSKnownSchemaVersion (..)
  , UnsupportedSchemaVersion (..)
  , classifySchemaVersion
    -- * Schema-indexed raw wire representation
  , RawDiagnostic (..)
  , SomeRawDiagnostic (..)
    -- * Per-version raw field sets
  , RawFieldsV1_0 (..)
  , RawFieldsV1_1 (..)
  , RawFieldsV1_2 (..)
  , RawSpan (..)
  , RawPosition (..)
  , RawReason (..)
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), withObject)
import Data.Aeson.Types ((.:))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Public, open SchemaVersion (§7: "should not imply that the currently
-- supported versions are the permanent universe of possible versions").
--------------------------------------------------------------------------------

newtype SchemaVersion = SchemaVersion Text
  deriving stock (Eq, Ord, Show)

-- | Total and unconditional: any 'Text' is a syntactically valid schema
-- version label, even one we don't support. Support is judged later, by
-- 'classifySchemaVersion', not here.
mkSchemaVersion :: Text -> SchemaVersion
mkSchemaVersion = SchemaVersion

unSchemaVersion :: SchemaVersion -> Text
unSchemaVersion (SchemaVersion t) = t

--------------------------------------------------------------------------------
-- Closed, known-version universe (type-level tag + singleton witness).
--------------------------------------------------------------------------------

-- | The closed universe of schema versions this implementation
-- understands. Promoted via DataKinds; used only as a type index, never
-- as a value.
data SchemaVersionTag = V1_0 | V1_1 | V1_2
  deriving stock (Eq, Show)

-- | Singleton witness for a known schema version. One constructor per
-- tag, so pattern-matching on this type is exhaustively checked by GHC
-- against 'SchemaVersionTag's constructor set.
data SKnownSchemaVersion (v :: SchemaVersionTag) where
  SV1_0 :: SKnownSchemaVersion 'V1_0
  SV1_1 :: SKnownSchemaVersion 'V1_1
  SV1_2 :: SKnownSchemaVersion 'V1_2

deriving stock instance Show (SKnownSchemaVersion v)
deriving stock instance Eq   (SKnownSchemaVersion v)

data SomeSKnownSchemaVersion where
  SomeSKnownSchemaVersion :: SKnownSchemaVersion v -> SomeSKnownSchemaVersion

newtype UnsupportedSchemaVersion = UnsupportedSchemaVersion SchemaVersion
  deriving stock (Eq, Show)

-- | Total: every 'SchemaVersion' is either one of the three known labels
-- or falls through to the single, deliberate catch-all — the only
-- allowed catch-all in this codebase, justified because 'SchemaVersion'
-- is an intentionally open type (§7). This is the mechanism behind §7's
-- dispatch diagram ("1.0 / 1.1 / 1.2 / future version"): the wildcard
-- branch produces data ('UnsupportedSchemaVersion'), never a decode-time
-- crash (invariant #2, §32).
classifySchemaVersion
  :: SchemaVersion -> Either UnsupportedSchemaVersion SomeSKnownSchemaVersion
classifySchemaVersion sv@(SchemaVersion t) = case t of
  "1.0" -> Right (SomeSKnownSchemaVersion SV1_0)
  "1.1" -> Right (SomeSKnownSchemaVersion SV1_1)
  "1.2" -> Right (SomeSKnownSchemaVersion SV1_2)
  _     -> Left (UnsupportedSchemaVersion sv)

--------------------------------------------------------------------------------
-- Schema-indexed raw wire representation (§8).
--------------------------------------------------------------------------------

-- | Each constructor carries exactly the fields that exist in that
-- schema version — nothing is a Maybe merely because a later version
-- happens to add it.
data RawDiagnostic (v :: SchemaVersionTag) where
  RawDiagnosticV1_0 :: RawFieldsV1_0 -> RawDiagnostic 'V1_0
  RawDiagnosticV1_1 :: RawFieldsV1_1 -> RawDiagnostic 'V1_1
  RawDiagnosticV1_2 :: RawFieldsV1_2 -> RawDiagnostic 'V1_2

-- | "Some raw diagnostic, together with the schema-version witness that
-- proves which constructor of 'RawDiagnostic' it is." The only place in
-- the codebase an existential is needed, because it is the only place a
-- runtime value (parsed JSON) must be married to a compile-time-checked
-- dispatch.
data SomeRawDiagnostic where
  SomeRawDiagnostic :: SKnownSchemaVersion v -> RawDiagnostic v -> SomeRawDiagnostic

--------------------------------------------------------------------------------
-- Per-version raw field sets (confirmed field shapes: spec Phase 1.3).
--------------------------------------------------------------------------------

data RawFieldsV1_0 = RawFieldsV1_0
  { rf10Version  :: Text
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
  , rf11Reason   :: Maybe RawReason  -- ^ new in 1.1
  } deriving stock (Eq, Show)

data RawFieldsV1_2 = RawFieldsV1_2
  { rf12Version  :: Text
  , rf12Span     :: Maybe RawSpan
  , rf12Severity :: Text
  , rf12Code     :: Maybe Integer
  , rf12Message  :: [Text]
  , rf12Hints    :: [Text]
  , rf12Reason   :: Maybe RawReason
  , rf12Rendered :: Text             -- ^ CONFIRMED required under schema 1.2 (not
                                     -- optional): GHC's own source unconditionally
                                     -- includes it ("rendered", JSString rendered),
                                     -- unlike "code"'s maybe-wrapped encoding --
                                     -- verified against the ticket #26173 commit
                                     -- that introduces this field and bumps
                                     -- schemaVersion to "1.2" in the same change.
  } deriving stock (Eq, Show)

-- | Nested, per GHC's confirmed wire shape: span -> {file, start, end}.
-- Deliberately not flattened, so a nesting-level mistake in the Aeson
-- parser is a type error, not merely a wrong-field-name bug.
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

--------------------------------------------------------------------------------
-- FromJSON instances for the version-independent nested wire shapes.
-- Kept here (not in Decode.hs) so they are not orphan instances (§12
-- style rule's spirit, applied beyond just Tadka.Diagnostic instances).
--------------------------------------------------------------------------------

instance FromJSON RawPosition where
  parseJSON = withObject "RawPosition" $ \o ->
    RawPosition <$> o .: "line" <*> o .: "column"

instance FromJSON RawSpan where
  parseJSON = withObject "RawSpan" $ \o ->
    RawSpan <$> o .: "file" <*> o .: "start" <*> o .: "end"

-- | Total over the oneOf's two shapes: tries "flags" first, falls back to
-- "category". A wire object satisfying neither shape fails via the
-- underlying Parser's own Alternative failure, surfaced by the caller as
-- a DecodeError, never as an uncaught pattern-match failure.
instance FromJSON RawReason where
  parseJSON = withObject "RawReason" $ \o ->
        (RawReasonFlags    <$> o .: "flags")
    <|> (RawReasonCategory <$> o .: "category")
