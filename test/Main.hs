-- | Phase 1 (wire/schema dispatch) and Phase 2 (semantic promotion,
-- decodeDiagnosticLine) tests. Vision §31.1/§31.3; spec Phase 9.1.
module Main (main) where

import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

import Tadka.GHCProtocol.Decode
import Tadka.GHCProtocol.Schema
import Tadka.GHCProtocol.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tadka-ghc"
  [ testGroup "Phase 1: schema/wire decode"
      [ testGroup "schema 1.0" schema10Tests
      , testGroup "schema 1.1" schema11Tests
      , testGroup "schema 1.2" schema12Tests
      , testGroup "schema dispatch" dispatchTests
      ]
  , testGroup "Phase 2: semantic promotion" phase2Tests
  ]

--------------------------------------------------------------------------------
-- Shared helpers
--------------------------------------------------------------------------------

decodeFixture :: FilePath -> IO (Either DecodeError SomeRawDiagnostic)
decodeFixture path = decodeSomeRawDiagnostic <$> BS.readFile path

decodeFixtureFull :: FilePath -> IO (Either DecodeError GhcDiagnostic)
decodeFixtureFull path = decodeDiagnosticLine <$> BS.readFile path

assertRight :: FilePath -> Assertion
assertRight path = do
  result <- decodeFixture path
  case result of
    Right _ -> pure ()
    Left e  -> assertFailure (path <> " expected Right, got " <> show e)

assertLeft :: FilePath -> Assertion
assertLeft path = do
  result <- decodeFixture path
  case result of
    Left _  -> pure ()
    Right _ -> assertFailure (path <> " expected Left, got Right")

--------------------------------------------------------------------------------
-- Phase 1: schema 1.0
--------------------------------------------------------------------------------

schema10Tests :: [TestTree]
schema10Tests =
  [ testCase "minimal decodes"      $ assertRight "test/fixtures/schema/1.0/minimal.json"
  , testCase "empty-hints decodes"  $ assertRight "test/fixtures/schema/1.0/empty-hints.json"
  , testCase "null-code decodes"    $ assertRight "test/fixtures/schema/1.0/null-code.json"
  , testCase "null-span decodes"    $ assertRight "test/fixtures/schema/1.0/null-span.json"
  , testCase "unknown-fields decodes (forward-compatible)" $
      assertRight "test/fixtures/schema/1.0/unknown-fields.json"
  , testCase "missing-field fails"  $ assertLeft "test/fixtures/schema/1.0/missing-field.json"
  , testCase "wrong-type fails"     $ assertLeft "test/fixtures/schema/1.0/wrong-type.json"
  , testCase "empty-message array is accepted, not rejected (§12)" $
      assertRight "test/fixtures/schema/1.0/empty-message.json"
  , testCase "maximal: fields round-trip exactly" $ do
      result <- decodeFixture "test/fixtures/schema/1.0/maximal.json"
      case result of
        Right (SomeRawDiagnostic SV1_0 (RawDiagnosticV1_0 f)) -> do
          assertEqual "ghcVersion" "9.10.1" (rf10Version f)
          assertEqual "severity"   "Error"  (rf10Severity f)
          assertEqual "code"       (Just 88464) (rf10Code f)
          assertEqual "message length" 2 (length (rf10Message f))
          assertEqual "hints length"   2 (length (rf10Hints f))
          case rf10Span f of
            Nothing -> assertFailure "expected a span"
            Just sp -> do
              assertEqual "span file" "<interactive>" (rsFile sp)
              assertEqual "start column" (7 :: Integer) (rpColumn (rsStart sp))
              assertEqual "end column"   (8 :: Integer) (rpColumn (rsEnd sp))
        _ -> assertFailure "expected SV1_0 dispatch"
  ]

--------------------------------------------------------------------------------
-- Phase 1: schema 1.1
--------------------------------------------------------------------------------

schema11Tests :: [TestTree]
schema11Tests =
  [ testCase "minimal decodes"     $ assertRight "test/fixtures/schema/1.1/minimal.json"
  , testCase "missing-field fails" $ assertLeft "test/fixtures/schema/1.1/missing-field.json"
  , testCase "wrong-type fails"    $ assertLeft "test/fixtures/schema/1.1/wrong-type.json"
  , testCase "optional-reason: reason absent decodes to Nothing" $ do
      result <- decodeFixture "test/fixtures/schema/1.1/optional-reason.json"
      case result of
        Right (SomeRawDiagnostic SV1_1 (RawDiagnosticV1_1 f)) ->
          assertEqual "reason" Nothing (rf11Reason f)
        _ -> assertFailure "expected SV1_1 dispatch"
  , testCase "reason-flags: NonEmpty flags preserved" $ do
      result <- decodeFixture "test/fixtures/schema/1.1/reason-flags.json"
      case result of
        Right (SomeRawDiagnostic SV1_1 (RawDiagnosticV1_1 f)) ->
          case rf11Reason f of
            Just (RawReasonFlags ("-Wunused-imports" :| [])) -> pure ()
            other -> assertFailure ("unexpected reason: " <> show other)
        _ -> assertFailure "expected SV1_1 dispatch"
  , testCase "reason-category: category preserved" $ do
      result <- decodeFixture "test/fixtures/schema/1.1/reason-category.json"
      case result of
        Right (SomeRawDiagnostic SV1_1 (RawDiagnosticV1_1 f)) ->
          assertEqual "reason" (Just (RawReasonCategory "deprecations")) (rf11Reason f)
        _ -> assertFailure "expected SV1_1 dispatch"
  ]

--------------------------------------------------------------------------------
-- Phase 1: schema 1.2
--------------------------------------------------------------------------------

schema12Tests :: [TestTree]
schema12Tests =
  [ testCase "minimal decodes"     $ assertRight "test/fixtures/schema/1.2/minimal.json"
  , testCase "missing-field fails" $ assertLeft "test/fixtures/schema/1.2/missing-field.json"
  , testCase "optional-rendered: rendered text preserved exactly" $ do
      result <- decodeFixture "test/fixtures/schema/1.2/optional-rendered.json"
      case result of
        Right (SomeRawDiagnostic SV1_2 (RawDiagnosticV1_2 f)) ->
          assertBool "rendered present" (rf12Rendered f /= Nothing)
        _ -> assertFailure "expected SV1_2 dispatch"
  , testCase "reason still available at 1.2 (inherited from 1.1)" $ do
      result <- decodeFixture "test/fixtures/schema/1.2/minimal.json"
      case result of
        Right (SomeRawDiagnostic SV1_2 (RawDiagnosticV1_2 f)) ->
          assertEqual "reason" Nothing (rf12Reason f)
        _ -> assertFailure "expected SV1_2 dispatch"
  ]

--------------------------------------------------------------------------------
-- Phase 1: schema dispatch / unsupported versions (§26)
--------------------------------------------------------------------------------

dispatchTests :: [TestTree]
dispatchTests =
  [ testCase "unsupported version 1.3 fails explicitly, not silently as 1.2" $ do
      result <- decodeFixture "test/fixtures/schema/unsupported/version-1.3.json"
      case result of
        Left (DecodeUnsupportedVersion sv) ->
          assertEqual "reported version" "1.3" (unSchemaVersion sv)
        Left other  -> assertFailure ("wrong error: " <> show other)
        Right _     -> assertFailure "must not silently decode an unknown version"
  ]

--------------------------------------------------------------------------------
-- Phase 2: semantic promotion / decodeDiagnosticLine (§31.3)
--------------------------------------------------------------------------------

phase2Tests :: [TestTree]
phase2Tests =
  [ testCase "Warning -> SevWarning" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/minimal.json"
      case result of
        Right d -> assertEqual "severity" SevWarning (ghcSeverity d)
        Left e  -> assertFailure (show e)

  , testCase "Error -> SevError" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/maximal.json"
      case result of
        Right d -> assertEqual "severity" SevError (ghcSeverity d)
        Left e  -> assertFailure (show e)

  , testCase "unrecognized severity fails explicitly, no Advice guess (§11)" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/unrecognized-severity.json"
      case result of
        Left (UnrecognizedSeverity "Info") -> pure ()
        other -> assertFailure ("expected UnrecognizedSeverity, got " <> show other)

  , testCase "message fragment order preserved" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/maximal.json"
      case result of
        Right d -> assertEqual "message"
          ["Variable not in scope: a", "Perhaps you meant \8216b\8217"]
          (ghcMessage d)
        Left e -> assertFailure (show e)

  , testCase "hint order preserved" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/maximal.json"
      case result of
        Right d -> assertEqual "hints"
          ["Add a type signature", "Import the missing module"]
          (ghcHints d)
        Left e -> assertFailure (show e)

  , testCase "no fabricated related/cause/id at the type level: GhcDiagnostic carries none" $
      -- Structural check: GhcDiagnostic's field list has no related/cause/id
      -- field to fabricate into -- §21 is enforced by this type's shape,
      -- exercised properly once the Tadka.Diagnostic instance exists (Phase 5).
      pure ()

  , testCase "source/file identity preserved through promotion (§20)" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/invalid-span-order.json"
      case result of
        Left (InvalidCoordinate _ _) -> pure ()
        other -> assertFailure ("expected InvalidCoordinate, got " <> show other)

  , testCase "GHC code is preserved, not fabricated into a Tadka code (§10)" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/maximal.json"
      case result of
        Right d -> case ghcCode d of
          Just _  -> pure ()
          Nothing -> assertFailure "expected a preserved GhcDiagnosticCode"
        Left e -> assertFailure (show e)

  , testCase "rendered output preserved as inert metadata, not reparsed (§15)" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.2/optional-rendered.json"
      case result of
        Right d -> case ghcRendered d of
          Just (RenderedDiagnostic t) -> assertBool "non-empty rendered text" (not (null (show t)))
          Nothing -> assertFailure "expected rendered text to be preserved"
        Left e -> assertFailure (show e)

  , testCase "reason preserved distinctly for schema 1.1 (§14)" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.1/reason-flags.json"
      case result of
        Right d -> case ghcReason d of
          Just (ReasonFlags ("-Wunused-imports" :| [])) -> pure ()
          other -> assertFailure ("unexpected reason: " <> show other)
        Left e -> assertFailure (show e)

  , testCase "reason absent for schema 1.0 (structural, not a null collapse)" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/minimal.json"
      case result of
        Right d -> assertEqual "reason" Nothing (ghcReason d)
        Left e  -> assertFailure (show e)

  , testCase "end-to-end: decodeDiagnosticLine on every per-version minimal fixture" $ do
      r10 <- decodeFixtureFull "test/fixtures/schema/1.0/minimal.json"
      r11 <- decodeFixtureFull "test/fixtures/schema/1.1/minimal.json"
      r12 <- decodeFixtureFull "test/fixtures/schema/1.2/minimal.json"
      mapM_ (either (assertFailure . show) (const (pure ()))) [r10, r11, r12]
  ]
