-- | Phase 1 (wire/schema dispatch), Phase 2 (semantic promotion,
-- decodeDiagnosticLine), Phase 3 (coordinate conversion, source
-- binding), and Phase 5 (Tadka projection) tests. Vision §31.1-§31.3;
-- spec Phase 9.1.
module Main (main) where

import qualified Data.ByteString as BS
import Test.Tasty.Golden (goldenVsString)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import Data.Functor.Identity (runIdentity)
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Aeson (Value (Number, String), eitherDecodeStrict)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import System.Exit (ExitCode (ExitFailure))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as Text
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import qualified Tadka
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  (Assertion, assertBool, assertEqual, assertFailure, testCase)

import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Hedgehog (Gen, Property, failure, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog (testProperty)

import CliTests (cliTests, timeoutTests)
import CoordinateTests (tabTests)
import Tadka.GHCProtocol.BuildTool
import Tadka.GHCProtocol.Decode
import Tadka.GHCProtocol.Diagnostic
import Tadka.GHCProtocol.Opaque
import Tadka.GHCProtocol.Process
import Tadka.GHCProtocol.Runner (BuildRunner (..), runBuild)
import Tadka.GHCProtocol.Schema
import Tadka.GHCProtocol.Span
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
  , testGroup "Phase 3: coordinate conversion / source binding" phase3Tests
  , testGroup "Phase 5: Tadka projection" phase5Tests
  , testGroup "Public accessors and human-readable rendering" accessorAndRenderingTests
  , testGroup "Typed build timeout" timeoutTests
  , testGroup "Command line" cliTests
  , testGroup "Tab columns and GHC coordinate conventions" tabTests
  , testGroup "Phase 6: build tool wrapper" phase6Tests
  , testGroup "Phase 7: opaque output capture" phase7Tests
  , testGroup "Phase 8: pure stream semantics" phase8Tests
  , testGroup "Phase 9: properties and deterministic build testing" phase9Tests
  , testGroup "Phase 2.7: decoding modes" decodingModeTests
  , testGroup "Phase 9: golden rendering" goldenTests
  , testGroup "Phase 9: coordinate fixtures" (map coordinateFixtureTest coordinateFixtureNames)
  , testGroup "Phase 9: additional properties" extraPropertyTests
  , testGroup "real-subprocess" realSubprocessTests
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

-- | Render a Doc to plain Text at a wide layout, for assertions -- never
-- pattern-matches on Doc's internal constructors (I-32's own discipline).
renderDocText :: Doc ann -> Text
renderDocText = renderStrict . layoutPretty defaultLayoutOptions

-- | Builds a validated 'GhcSpan' from raw 1-based coordinates, failing
-- the test (not the program) if the coordinates themselves are invalid.
buildSpanIO :: FilePath -> Integer -> Integer -> Integer -> Integer -> IO GhcSpan
buildSpanIO file sl sc el ec =
  case buildSpan file sl sc el ec of
    Right sp -> pure sp
    Left e   -> assertFailure ("buildSpanIO: " <> show e)

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
      result <- decodeFixture "test/fixtures/schema/1.2/rendered.json"
      case result of
        Right (SomeRawDiagnostic SV1_2 (RawDiagnosticV1_2 f)) ->
          assertBool "rendered present and non-empty" (not (Text.null (rf12Rendered f)))
        _ -> assertFailure "expected SV1_2 dispatch"

  , testCase "rendered: MISSING under schema 1.2 is a decode error, not silently absent" $ do
      result <- decodeFixture "test/fixtures/schema/1.2/missing-rendered.json"
      case result of
        Left _  -> pure ()
        Right _ -> assertFailure "expected a missing-field decode error; 'rendered' is required under 1.2"
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

  , testCase "a span whose end precedes its start is rejected with its real coordinates" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/invalid-span-order.json"
      case result of
        Left (InvalidSpanOrder (sl, sc) (el, ec)) ->
          assertEqual "start then end"
            [5, 10, 5, 3]
            [unLine sl, unColumn sc, unLine el, unColumn ec]
        other -> assertFailure ("expected InvalidSpanOrder, got " <> show other)

  , testCase "GHC code is preserved, not fabricated into a Tadka code (§10)" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/maximal.json"
      case result of
        Right d -> case ghcCode d of
          Just _  -> pure ()
          Nothing -> assertFailure "expected a preserved GhcDiagnosticCode"
        Left e -> assertFailure (show e)

  , testCase "rendered output preserved as inert metadata, not reparsed (§15)" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.2/rendered.json"
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

--------------------------------------------------------------------------------
-- Phase 3: coordinate conversion / source binding (§31.2, §18's table)
--------------------------------------------------------------------------------

phase3Tests :: [TestTree]
phase3Tests =
  [ testCase "computeLineMetadata: two plain lines" $ do
      let meta = computeLineMetadata "abc\ndef"
      case meta of
        (LineMetadata 0 "abc") :| [LineMetadata 4 "def"] -> pure ()
        other -> assertFailure ("unexpected metadata: " <> show other)

  , testCase "computeLineMetadata: CRLF excludes \\r from line content (§3.1)" $ do
      let meta = computeLineMetadata "abc\r\ndef"
      case meta of
        (LineMetadata 0 "abc") :| [LineMetadata 5 "def"] -> pure ()
        other -> assertFailure ("unexpected metadata: " <> show other)

  , testCase "computeLineMetadata: trailing newline yields an empty final line" $ do
      let meta = computeLineMetadata "abc\n"
      case meta of
        (LineMetadata 0 "abc") :| [LineMetadata 4 ""] -> pure ()
        other -> assertFailure ("unexpected metadata: " <> show other)

  , testCase "coordinateToOffset: first character of first line is offset 0" $ do
      let meta = computeLineMetadata "abc\ndef"
      l <- either (assertFailure . show) pure (mkLine 1)
      c <- either (assertFailure . show) pure (mkColumn 1)
      assertEqual "offset" (Right 0) (coordinateToOffset meta l c)

  , testCase "coordinateToOffset: start of second line" $ do
      let meta = computeLineMetadata "abc\ndef"
      l <- either (assertFailure . show) pure (mkLine 2)
      c <- either (assertFailure . show) pure (mkColumn 1)
      assertEqual "offset" (Right 4) (coordinateToOffset meta l c)

  , testCase "coordinateToOffset: line beyond source fails" $ do
      let meta = computeLineMetadata "abc\ndef"
      l <- either (assertFailure . show) pure (mkLine 99)
      c <- either (assertFailure . show) pure (mkColumn 1)
      case coordinateToOffset meta l c of
        Left (LineOutOfRange _) -> pure ()
        other -> assertFailure ("expected LineOutOfRange, got " <> show other)

  , testCase "coordinateToOffset: column beyond line length fails" $ do
      let meta = computeLineMetadata "abc\ndef"
      l <- either (assertFailure . show) pure (mkLine 1)
      c <- either (assertFailure . show) pure (mkColumn 99)
      case coordinateToOffset meta l c of
        Left (ColumnOutOfRange _ _) -> pure ()
        other -> assertFailure ("expected ColumnOutOfRange, got " <> show other)

  , testCase "coordinateToOffset: exclusive end-of-line column is legal (§3.1)" $ do
      let meta = computeLineMetadata "abc\ndef"
      l <- either (assertFailure . show) pure (mkLine 1)
      c <- either (assertFailure . show) pure (mkColumn 4) -- one past 'c'
      assertEqual "offset" (Right 3) (coordinateToOffset meta l c)

  , testCase "convertSpan: end-to-end success produces a real Tadka.Context" $ do
      sp <- buildSpanIO "Foo.hs" 1 5 1 6  -- the 'x' in "let x = 1"
      case convertSpan (SourceText "let x = 1\n") sp of
        Left err  -> assertFailure ("expected Right, got " <> show err)
        Right ctx -> case Tadka.contextLabelStates ctx of
          [Tadka.LabelOk _] -> pure ()
          other -> assertFailure ("unexpected label states: " <> show other)

  , testCase "convertSpan: coordinates outside the source fail with InvalidCoordinates" $ do
      sp <- buildSpanIO "Foo.hs" 99 1 99 2
      case convertSpan (SourceText "let x = 1\n") sp of
        Left (InvalidCoordinates _ _) -> pure ()
        other -> assertFailure ("expected InvalidCoordinates, got " <> show other)

  , testCase "bindSpan: Nothing -> NoSpan" $ do
      result <- bindSpan (SourceProvider (\_ -> pure (Right Nothing))) Nothing
      case result of
        NoSpan -> pure ()
        other  -> assertFailure ("expected NoSpan, got " <> show other)

  , testCase "bindSpan: clean not-found -> SpanNoSource, not an error (§2/§18)" $ do
      sp <- buildSpanIO "Missing.hs" 1 1 1 2
      result <- bindSpan (SourceProvider (\_ -> pure (Right Nothing))) (Just sp)
      case result of
        SpanNoSource _ -> pure ()
        other -> assertFailure ("expected SpanNoSource, got " <> show other)

  , testCase "bindSpan: genuine lookup failure -> SpanSourceUnavailable, distinct from not-found" $ do
      sp <- buildSpanIO "Foo.hs" 1 1 1 2
      let err = SourceIOError "permission denied"
      result <- bindSpan (SourceProvider (\_ -> pure (Left err))) (Just sp)
      case result of
        SpanSourceUnavailable _ e -> assertEqual "error" err e
        other -> assertFailure ("expected SpanSourceUnavailable, got " <> show other)

  , testCase "bindSpan: success -> SpanBound with a real Context" $ do
      sp <- buildSpanIO "Foo.hs" 1 5 1 6
      let provide _ = pure (Right (Just (SourceText "let x = 1\n")))
      result <- bindSpan (SourceProvider provide) (Just sp)
      case result of
        SpanBound ctx -> case Tadka.contextLabelStates ctx of
          [Tadka.LabelOk _] -> pure ()
          other -> assertFailure ("unexpected label states: " <> show other)
        other -> assertFailure ("expected SpanBound, got " <> show other)

  , testCase "fileSourceProvider: nonexistent file yields Right Nothing, not an exception" $ do
      result <- lookupSource fileSourceProvider "/nonexistent/path/definitely-not-there.hs"
      case result of
        Right Nothing -> pure ()
        other -> assertFailure ("expected Right Nothing, got " <> show other)
  ]

--------------------------------------------------------------------------------
-- Phase 5: Tadka projection (§31.3)
--------------------------------------------------------------------------------

phase5Tests :: [TestTree]
phase5Tests =
  [ testCase "Tadka.severity: Warning -> SevWarning" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/minimal.json"
      case result of
        Right d -> assertEqual "severity" Tadka.SevWarning (Tadka.severity d)
        Left e  -> assertFailure (show e)

  , testCase "Tadka.severity: Error -> SevError" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/maximal.json"
      case result of
        Right d -> assertEqual "severity" Tadka.SevError (Tadka.severity d)
        Left e  -> assertFailure (show e)

  , testCase "Tadka.code is always Nothing; ghcCode still preserves the real value (§10)" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/maximal.json"
      case result of
        Right d -> do
          assertEqual "Tadka.code" Nothing (Tadka.code d)
          assertBool "ghcCode preserved" (isJust (ghcCode d))
        Left e -> assertFailure (show e)

  , testCase "no fabricated related/id (§21)" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/minimal.json"
      case result of
        Right d -> do
          assertEqual "related length" 0 (length (Tadka.related d))
          assertEqual "diagnosticId" Nothing (Tadka.diagnosticId d)
        Left e -> assertFailure (show e)

  , testCase "GhcDiagnostic's own context is always NoContext" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/maximal.json"
      case result of
        Right d -> case Tadka.context d of
          Tadka.NoContext -> pure ()
          _               -> assertFailure "expected NoContext for a plain GhcDiagnostic"
        Left e -> assertFailure (show e)

  , testCase "messageDoc: fragment order preserved, one line per fragment" $
      assertEqual "rendered" "first\nsecond\nthird"
        (renderDocText (messageDoc ["first", "second", "third"]))

  , testCase "messageDoc: empty array renders to empty text (§12)" $
      assertEqual "rendered" "" (renderDocText (messageDoc []))

  , testCase "helpDoc: Nothing for empty hints" $
      assertEqual "helpDoc" Nothing (fmap renderDocText (helpDoc []))

  , testCase "helpDoc: order preserved" $
      case helpDoc ["first hint", "second hint"] of
        Nothing -> assertFailure "expected Just"
        Just d  -> assertEqual "rendered" "\8226 first hint\n\8226 second hint" (renderDocText d)

  , testCase "BoundGhcDiagnostic: context reflects SpanBound" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/minimal.json"
      case result of
        Left e -> assertFailure (show e)
        Right d -> do
          sp <- buildSpanIO "Foo.hs" 1 5 1 6
          case convertSpan (SourceText "let x = 1\n") sp of
            Left err  -> assertFailure (show err)
            Right ctx ->
              case Tadka.context (BoundGhcDiagnostic d (SpanBound ctx)) of
                Tadka.NoContext -> assertFailure "expected the bound context, not NoContext"
                _               -> pure ()

  , testCase "BoundGhcDiagnostic: NoSpan/SpanNoSource/etc. all degrade to NoContext" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/minimal.json"
      case result of
        Left e -> assertFailure (show e)
        Right d -> do
          sp <- buildSpanIO "Foo.hs" 1 1 1 2
          let states =
                [ NoSpan
                , SpanNoSource sp
                , SpanSourceUnavailable sp (SourceIOError "boom")
                , SpanInvalidCoordinates sp (InvalidCoordinates sp (LineOutOfRange (spanStartLine sp)))
                ]
          mapM_
            (\st -> case Tadka.context (BoundGhcDiagnostic d st) of
                Tadka.NoContext -> pure ()
                _               -> assertFailure ("expected NoContext for " <> show st))
            states

  , testCase "renderDecodeError is total and human-readable, not Show-derived" $
      assertBool "non-empty" (renderDecodeError DecodeNotAnObject /= "")

  , testCase "renderSourceBindingError is total and human-readable" $ do
      sp <- buildSpanIO "Foo.hs" 1 1 1 2
      assertBool "non-empty" (renderSourceBindingError (InvalidCoordinates sp (LineOutOfRange (spanStartLine sp))) /= "")
  ]
--------------------------------------------------------------------------------
-- Public accessors and human-readable rendering (audit B8, B10, B14)
--------------------------------------------------------------------------------

leftOrFail :: String -> Either a b -> IO a
leftOrFail what = either pure (const (assertFailure (what <> ": expected Left, got Right")))

-- A tadka 'Tadka.ContextError' obtained the way production code obtains
-- one: a span reaching past the end of its source.
outOfBoundsContextError :: IO Tadka.ContextError
outOfBoundsContextError = do
  named <- either (const (assertFailure "mkNamedSource rejected a valid name")) pure
             (Tadka.mkNamedSource "Foo.hs" "abc")
  spn   <- either (const (assertFailure "mkSpan rejected a valid span")) pure
             (Tadka.mkSpan 0 10)
  leftOrFail "a span past the end of the source"
    (Tadka.mkContext named (Tadka.Labeled spn Tadka.Primary Nothing :| []))

accessorAndRenderingTests :: [TestTree]
accessorAndRenderingTests =
  [ testCase "a consumer can read the GHC version, code and span back out (§10)" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.1/maximal.json"
      case result of
        Left e  -> assertFailure (show e)
        Right d -> do
          assertEqual "version" "9.10.1" (unGhcVersion (ghcVersion d))
          assertEqual "code" (Just 88464) (fmap unGhcDiagnosticCode (ghcCode d))
          assertEqual "span" (Just "<interactive>:2:7-2:8") (fmap renderGhcSpan (ghcSpan d))

  , testCase "an inverted span reports both positions, with no sentinel value (B8)" $ do
      case buildSpan "Foo.hs" 3 4 3 2 of
        Left e@(InvalidSpanOrder _ _) ->
          assertEqual "rendering" "span ends at 3:2 before it starts at 3:4" (renderDecodeError e)
        other -> assertFailure ("expected InvalidSpanOrder, got " <> show other)

  , testCase "a zero-width span is accepted (B8)" $
      case buildSpan "Foo.hs" 3 4 3 4 of
        Right _ -> pure ()
        Left e  -> assertFailure (show e)

  , testCase "renderDecodeError never prints constructor or record syntax (B14)" $ do
      let sv = mkSchemaVersion "1.0"
          errs =
            [ DecodeMalformedJson "unexpected end"
            , DecodeNotAnObject
            , DecodeMissingField sv "hints"
            , DecodeFieldTypeMismatch sv "code" "integer"
            , DecodeUnsupportedVersion sv
            , DecodeUnknownField sv "extra"
            , InvalidGhcVersion ""
            , InvalidDiagnosticCode (-1)
            , UnrecognizedSeverity "Note"
            , InvalidCoordinate "line numbers are 1-based; got " 0
            ]
          leaks t = any (`Text.isInfixOf` t)
                      ["Decode", "SchemaVersion", "Invalid", "Unrecognized", "{", "}"]
      mapM_ (\e -> let t = renderDecodeError e
                    in assertBool ("leaks Show syntax: " <> Text.unpack t) (not (leaks t)))
            errs

  , testCase "renderSourceBindingError never prints record syntax (B14)" $ do
      sp      <- buildSpanIO "Foo.hs" 1 2 3 4
      offErr  <- leftOrFail "negative offset" (Tadka.mkSpan (-1) 0)
      lenErr  <- leftOrFail "negative length" (Tadka.mkSpan 0 (-1))
      srcErr  <- leftOrFail "empty source name" (Tadka.mkNamedSource "" "x")
      ctxErr  <- outOfBoundsContextError
      let rendered =
            [ renderSourceBindingError (InvalidCoordinates sp (LineOutOfRange (spanStartLine sp)))
            , renderSourceBindingError (TadkaSpanRejected sp offErr)
            , renderSourceBindingError (TadkaSpanRejected sp lenErr)
            , renderSourceBindingError (TadkaSourceRejected sp srcErr)
            , renderSourceBindingError (TadkaContextRejected sp ctxErr)
            ]
          leaks t = any (`Text.isInfixOf` t)
                      ["GhcSpan {", "Line ", "Column ", "SpanBad", "EmptySourceName", "ContextError"]
      mapM_ (\t -> assertBool ("leaks Show syntax: " <> Text.unpack t) (not (leaks t))) rendered
      assertBool "names the span"
        (all ("Foo.hs:1:2-3:4" `Text.isInfixOf`) rendered)

  , testCase "an out-of-bounds binding error states both numbers (B14)" $ do
      sp     <- buildSpanIO "Foo.hs" 1 1 1 2
      ctxErr <- outOfBoundsContextError
      assertEqual "message"
        "span out of bounds against real source for Foo.hs:1:1-1:2: \
        \the span ends at character 10 but the source has only 3 characters"
        (renderSourceBindingError (TadkaContextRejected sp ctxErr))
  ]

--------------------------------------------------------------------------------
-- Phase 6: build tool detection, flag injection, chunk framing (§31.5)
--------------------------------------------------------------------------------

phase6Tests :: [TestTree]
phase6Tests =
  [ testCase "detectBuildTool: explicit override bypasses detection entirely" $ do
      result <- detectBuildTool (Just Stack) "test/fixtures/build/cabal-only"
      assertEqual "detected" (Right Stack) result

  , testCase "detectBuildTool: a nonexistent directory is a typed error, not an uncaught exception (regression)" $ do
      -- Found via manual CLI testing: without explicit exception handling,
      -- this crashed with an uncaught IOException instead of the typed
      -- BuildToolDetectionError this function's type otherwise promises.
      result <- detectBuildTool Nothing "/this/path/does/not/exist"
      case result of
        Left (ProjectDirectoryUnreadable dir _) ->
          assertEqual "path recorded" "/this/path/does/not/exist" dir
        other -> assertFailure ("expected ProjectDirectoryUnreadable, got " <> show other)

  , testCase "detectBuildTool: cabal-only fixture selects Cabal" $ do
      result <- detectBuildTool Nothing "test/fixtures/build/cabal-only"
      assertEqual "detected" (Right Cabal) result

  , testCase "detectBuildTool: stack.yaml wins even alongside a .cabal file" $ do
      result <- detectBuildTool Nothing "test/fixtures/build/stack-only"
      assertEqual "detected" (Right Stack) result

  , testCase "detectBuildTool: more than one .cabal file is ambiguous" $ do
      result <- detectBuildTool Nothing "test/fixtures/build/ambiguous"
      case result of
        Left (AmbiguousProjectFiles _) -> pure ()
        other -> assertFailure ("expected AmbiguousProjectFiles, got " <> show other)

  , testCase "detectBuildTool: no recognized files fails explicitly" $ do
      result <- detectBuildTool Nothing "test/fixtures/build/none"
      assertEqual "detected" (Left NoRecognizedProjectFile) result

  , testCase "classifyGhcFlag: -fno-diagnostics-as-json is NOT a substring false-positive" $
      assertEqual "classified" (Just DisableDiagnosticsJson)
        (classifyGhcFlag "-fno-diagnostics-as-json")

  , testCase "classifyGhcFlag: enable flag recognized" $
      assertEqual "classified" (Just EnableDiagnosticsJson)
        (classifyGhcFlag "-fdiagnostics-as-json")

  , testCase "extractGhcOptionTokens: multiple --ghc-options accumulate, not override" $
      assertEqual "tokens"
        ["-Wall", "-fdiagnostics-as-json", "-O2"]
        (extractGhcOptionTokens ["--ghc-options=-Wall -fdiagnostics-as-json", "--ghc-options=-O2"])

  , testCase "diagnosticsJsonCurrentlyEnabled: last occurrence wins" $
      assertBool "enabled"
        (diagnosticsJsonCurrentlyEnabled
          ["--ghc-options=-fno-diagnostics-as-json", "--ghc-options=-fdiagnostics-as-json"])

  , testCase "diagnosticsJsonCurrentlyEnabled: absent means not enabled" $
      assertBool "not enabled" (not (diagnosticsJsonCurrentlyEnabled ["--ghc-options=-Wall"]))

  , testCase "injectDiagnosticsFlag: present after injection, even with an explicit -fno- first" $ do
      let args = injectDiagnosticsFlag Cabal ["--ghc-options=-fno-diagnostics-as-json"]
      assertBool "enabled after injection" (diagnosticsJsonCurrentlyEnabled args)

  , testCase "injectDiagnosticsFlag: does not remove the user's own flag text" $ do
      let original = "--ghc-options=-fno-diagnostics-as-json"
          args      = injectDiagnosticsFlag Cabal [original]
      assertBool "original text preserved" (original `elem` args)

  , testCase "injectDiagnosticsFlag: idempotent" $ do
      let once  = injectDiagnosticsFlag Cabal ["--ghc-options=-Wall"]
          twice = injectDiagnosticsFlag Cabal once
      assertEqual "idempotent" once twice

  , testCase "injectDiagnosticsFlag: already-enabled args are left untouched (no redundant duplicate)" $ do
      let args = ["--ghc-options=-fdiagnostics-as-json"]
      assertEqual "unchanged" args (injectDiagnosticsFlag Cabal args)

  , testCase "feedChunk/flushFramer: single chunk containing multiple lines" $ do
      let (st, lns) = feedChunk emptyFramerState "line1\nline2\nline3\n"
      assertEqual "lines" ["line1", "line2", "line3"] lns
      assertEqual "flush after trailing newline" [] (flushFramer st)

  , testCase "feedChunk: a line split across two chunks reconstructs correctly" $ do
      let (st1, lns1) = feedChunk emptyFramerState "partial-li"
          (_st2, lns2) = feedChunk st1 "ne\nnext\n"
      assertEqual "first chunk yields no complete lines" [] lns1
      assertEqual "second chunk completes the split line" ["partial-line", "next"] lns2

  , testCase "flushFramer: recovers a final unterminated line at EOF" $ do
      let (st, lns) = feedChunk emptyFramerState "no-trailing-newline"
      assertEqual "no complete lines yet" [] lns
      assertEqual "flushed" ["no-trailing-newline"] (flushFramer st)

  , testCase "splitCompleteLines: reconstructs input split at every possible boundary" $ do
      let whole = "abc\ndef\nghi"
          (wholeLines, wholeRest) = splitCompleteLines whole
      -- reference: splitting in one piece
      assertEqual "reference split" (["abc", "def"], "ghi") (wholeLines, wholeRest)
  ]
--------------------------------------------------------------------------------
-- Phase 7: opaque output capture / line classification (§31.5)
--------------------------------------------------------------------------------

validDiagLine :: BS.ByteString
validDiagLine =
  "{\"version\":\"1.0\",\"ghcVersion\":\"9.10.1\",\"span\":null,\"severity\":\"Warning\",\"code\":null,\"message\":[\"msg\"],\"hints\":[]}"

panicLine1, panicLine2, blankLine, progressLine :: BS.ByteString
panicLine1   = "ghc: panic! (the 'impossible' happened)"
panicLine2   = "  GHC version 9.10.1:"
blankLine    = ""
progressLine = "cabal: Resolving dependencies..."

phase7Tests :: [TestTree]
phase7Tests =
  [ testCase "a GHC panic with no diagnostic JSON produces ClassifiedOpaque, not silently dropped" $ do
      let result = classifyStream StdErr [panicLine1, panicLine2, blankLine]
      case result of
        [ClassifiedOpaque o] ->
          assertBool "raw bytes contain both panic lines"
            (BS.isInfixOf panicLine1 (opaqueRawBytes o) && BS.isInfixOf panicLine2 (opaqueRawBytes o))
        other -> assertFailure ("expected a single ClassifiedOpaque block, got " <> show other)

  , testCase "a panic block with no trailing blank line at EOF is still captured (finalize)" $ do
      let result = classifyStream StdErr [panicLine1, panicLine2]
      case result of
        [ClassifiedOpaque _] -> pure ()
        other -> assertFailure ("expected a finalized ClassifiedOpaque block, got " <> show other)

  , testCase "mixed valid-diagnostic and non-JSON lines classify each line correctly" $ do
      let result = classifyStream StdErr [validDiagLine, progressLine, panicLine1, blankLine]
      case result of
        [ClassifiedDiagnostic _, ClassifiedOpaque progressRecord, ClassifiedOpaque _] ->
          assertEqual "progress text preserved verbatim" progressLine (opaqueRawBytes progressRecord)
        other -> assertFailure ("unexpected classification shape: " <> show other)

  , testCase "cabal's own progress text is never misclassified as a GHC diagnostic" $
      case classifyStream StdErr [progressLine] of
        [ClassifiedOpaque _] -> pure ()
        other -> assertFailure ("expected ClassifiedOpaque, got " <> show other)

  , testCase "classifyInterleaved: a pending panic on one stream doesn't disturb the other" $ do
      let input =
            [ (StdErr, panicLine1)
            , (StdOut, validDiagLine)
            , (StdErr, blankLine)
            ]
      case classifyInterleaved input of
        [ClassifiedDiagnostic _, ClassifiedOpaque _] -> pure ()
        other -> assertFailure ("unexpected interleaved classification: " <> show other)

  , testCase "attachCompilerResult: CompilerSucceeded leaves classifications untouched" $ do
      let cs = classifyStream StdOut [validDiagLine, progressLine]
      assertEqual "unchanged" cs (attachCompilerResult CompilerSucceeded cs)

  , testCase "attachCompilerResult: failure sets Just result on every opaque, leaves diagnostics alone" $ do
      let cs = classifyStream StdOut [validDiagLine, progressLine]
          result = CompilerFailed (ExitFailure 1)
          attached = attachCompilerResult result cs
      case attached of
        [ClassifiedDiagnostic _, ClassifiedOpaque o] ->
          assertEqual "opaqueCompilerResult set" (Just result) (opaqueCompilerResult o)
        other -> assertFailure ("unexpected shape: " <> show other)

  , testCase "attachCompilerResult: non-zero exit with zero diagnostics still yields one opaque record" $ do
      let result = CompilerFailed (ExitFailure 1)
      case attachCompilerResult result [] of
        [ClassifiedOpaque o] -> assertEqual "compiler result" (Just result) (opaqueCompilerResult o)
        other -> assertFailure ("expected one synthetic ClassifiedOpaque, got " <> show other)

  , testCase "classifyBuildOutput: buildCompilerResult reflects failure even when every line decoded fine" $ do
      let br = BuildResult (CompilerFailed (ExitFailure 1)) [(StdOut, validDiagLine)]
          outcome = classifyBuildOutput br
      assertEqual "buildCompilerResult" (CompilerFailed (ExitFailure 1)) (buildCompilerResult outcome)

  , testCase "OpaqueGhcOutput: Tadka.severity is always SevError" $ do
      let o = OpaqueGhcOutput StdErr "boom" "boom" Nothing
      assertEqual "severity" Tadka.SevError (Tadka.severity o)

  , testCase "OpaqueGhcOutput: Tadka.context is NoContext (no span ever existed)" $ do
      let o = OpaqueGhcOutput StdErr "boom" "boom" Nothing
      case Tadka.context o of
        Tadka.NoContext -> pure ()
        _               -> assertFailure "expected NoContext"

  , testCase "OpaqueGhcOutput's projection is never confused with GhcDiagnostic's (distinct types)" $ do
      -- The real guarantee here is at the type level: OpaqueGhcOutput and
      -- GhcDiagnostic are distinct types with independent Diagnostic
      -- instances, so no call site can accidentally substitute one for
      -- the other. This test just exercises both concretely.
      diagResult <- decodeFixtureFull "test/fixtures/schema/1.0/minimal.json"
      case diagResult of
        Left e -> assertFailure (show e)
        Right d -> do
          let o = OpaqueGhcOutput StdErr "x" "x" Nothing
          assertBool "diag severity may be Warning" (Tadka.severity d == Tadka.SevWarning)
          assertBool "opaque severity is always Error" (Tadka.severity o == Tadka.SevError)

  , testCase "isPanicMarker: recognizes the prefix regardless of trailing summary text" $
      assertBool "matches" (isPanicMarker "ghc: panic! (the 'impossible' happened)\n  GHC version 9.10.1:")

  , testCase "isPanicMarker: does not match ordinary output" $
      assertBool "no match" (not (isPanicMarker progressLine))

  , testCase "KNOWN LIMITATION: a diagnostic-pipeline-wrapped panic is not grouped, but is NOT dropped either" $ do
      -- Documents a real, confirmed gap found while verifying isPanicMarker
      -- against GHC's source: this form (no leading "ghc: ", indented,
      -- prefixed by a separate "<no location info>: error:" line) is not
      -- recognised as a panic START. Each line still becomes its own
      -- ClassifiedOpaque record -- completeness (§4) holds; only grouping
      -- into one coherent block does not.
      contents <- BS.readFile "test/fixtures/build/panic-wrapped/lines.txt"
      let ls = filter (not . BS.null) (BSC.split '\n' contents)
          results = classifyStream StdErr ls
      assertEqual "one record per line (not merged into one block)" (length ls) (length results)
      assertBool "every line is still captured as opaque, none lost"
        (all (\r -> case r of ClassifiedOpaque _ -> True; _ -> False) results)
  ]
--------------------------------------------------------------------------------
-- Phase 8: pure stream semantics over known-diagnostic JSON Lines (§23)
--------------------------------------------------------------------------------

phase8Tests :: [TestTree]
phase8Tests =
  [ testCase "decodeDiagnosticStream: multiple valid lines all decode" $ do
      let ls = [validDiagLine, validDiagLine]
      assertEqual "count" 2 (length (decodeDiagnosticStream ls))
      assertBool "all Right" (all isRightE (decodeDiagnosticStream ls))

  , testCase "decodeDiagnosticStream: blank lines are skipped, not errors" $ do
      let ls = [validDiagLine, "", validDiagLine]
      assertEqual "only 2 records, blank skipped" 2 (length (decodeDiagnosticStream ls))

  , testCase "decodeDiagnosticStream: CRLF line ending is normalized before decoding" $ do
      let crlfLine = validDiagLine <> "\r"
      case decodeDiagnosticStream [crlfLine] of
        [Right _] -> pure ()
        other -> assertFailure ("expected a single Right after CRLF stripping, got " <> show (length other))

  , testCase "decodeDiagnosticStream: a malformed record does not obscure others' results" $ do
      let ls = [validDiagLine, "not json at all", validDiagLine]
          results = decodeDiagnosticStream ls
      assertEqual "3 non-blank lines -> 3 results" 3 (length results)
      case results of
        [Right _, Left _, Right _] -> pure ()
        other -> assertFailure ("unexpected result shape: " <> show (map isRightE other))

  , testCase "decodeDiagnosticStream: length is at most input length, equal iff no blank lines" $ do
      let withBlank    = [validDiagLine, "", validDiagLine]
          withoutBlank  = [validDiagLine, validDiagLine]
      assertBool "with blank: fewer results than input lines"
        (length (decodeDiagnosticStream withBlank) < length withBlank)
      assertEqual "without blank: equal count"
        (length withoutBlank) (length (decodeDiagnosticStream withoutBlank))
  ]
  where
    isRightE (Right _) = True
    isRightE (Left _)  = False
--------------------------------------------------------------------------------
-- Phase 9: Hedgehog property layer (§31.4) and BuildRunner-based
-- deterministic build-tool testing (§9.4). Named for direct
-- traceability against the spec's own property list where applicable.
--------------------------------------------------------------------------------

-- | ASCII letters only, no embedded newlines -- keeps messageDoc's
-- line-per-fragment property meaningful (a fragment containing its own
-- '\n' would make naive line-splitting ambiguous, which is a property
-- about Text.splitOn, not about messageDoc's own fragment handling).
genSafeText :: Gen Text.Text
genSafeText = Gen.text (Range.linear 0 12) Gen.alpha

-- | A GhcDiagnostic built directly (not via JSON decode), using
-- Gen.discard for the rare case a generated value fails a smart
-- constructor's validation -- the standard, total Hedgehog idiom for a
-- generator with a precondition, not a partial function.
genGhcDiagnostic :: Gen GhcDiagnostic
genGhcDiagnostic = do
  verTxt <- Gen.text (Range.linear 1 8) Gen.alpha
  ver <- case mkGhcVersion verTxt of
    Right v -> pure v
    Left _  -> Gen.discard
  sev <- Gen.element [SevWarning, SevError]
  mCodeInt <- Gen.maybe (Gen.integral (Range.linear 0 999999))
  mCode <- case mCodeInt of
    Nothing -> pure Nothing
    Just n  -> case mkGhcDiagnosticCode n of
      Right c -> pure (Just c)
      Left _  -> Gen.discard
  msgs  <- Gen.list (Range.linear 0 5) genSafeText
  hints <- Gen.list (Range.linear 0 5) genSafeText
  pure GhcDiagnostic
    { ghcVersion = ver, ghcSpan = Nothing, ghcSeverity = sev, ghcCode = mCode
    , ghcMessage = msgs, ghcHints = hints, ghcReason = Nothing, ghcRendered = Nothing
    }

genBuildArgs :: Gen [Text.Text]
genBuildArgs = Gen.list (Range.linear 0 5) genArg
  where
    genArg = Gen.choice
      [ pure "--ghc-options=-fdiagnostics-as-json"
      , pure "--ghc-options=-fno-diagnostics-as-json"
      , pure "--ghc-options=-Wall"
      , (\t -> "--ghc-options=" <> t) <$> genSafeText
      ]

-- | A line guaranteed non-JSON (doesn't start with '{'), non-blank, and
-- not a panic marker -- so classifyStream is guaranteed to degrade it
-- to a single ClassifiedOpaque with the bytes preserved exactly.
genOpaqueSafeLine :: Gen BS.ByteString
genOpaqueSafeLine = do
  t <- Gen.text (Range.linear 1 12) Gen.alpha
  pure (encodeUtf8 ("x" <> t))

prop_messageDoc_preserves_order :: Property
prop_messageDoc_preserves_order = property $ do
  frags <- forAll (Gen.list (Range.linear 1 8) genSafeText)
  let rendered  = renderDocText (messageDoc frags)
      splitBack = Text.splitOn "\n" rendered
  splitBack === frags

prop_messageDoc_preserves_fragment_count :: Property
prop_messageDoc_preserves_fragment_count = property $ do
  frags <- forAll (Gen.list (Range.linear 1 8) genSafeText)
  let rendered = renderDocText (messageDoc frags)
  length (Text.splitOn "\n" rendered) === length frags

prop_code_projection_always_Nothing :: Property
prop_code_projection_always_Nothing = property $ do
  d <- forAll genGhcDiagnostic
  Tadka.code d === Nothing

prop_unsupported_version_always_Left :: Property
prop_unsupported_version_always_Left = property $ do
  t <- forAll (Gen.filter (`notElem` ["1.0", "1.1", "1.2"]) (Gen.text (Range.linear 1 6) Gen.alphaNum))
  case classifySchemaVersion (mkSchemaVersion t) of
    Left _  -> pure ()
    Right _ -> failure

prop_flag_always_present_after_injection :: Property
prop_flag_always_present_after_injection = property $ do
  args <- forAll genBuildArgs
  let injected = injectDiagnosticsFlag Cabal args
  diagnosticsJsonCurrentlyEnabled injected === True

prop_injection_idempotent :: Property
prop_injection_idempotent = property $ do
  args <- forAll genBuildArgs
  let once  = injectDiagnosticsFlag Cabal args
      twice = injectDiagnosticsFlag Cabal once
  once === twice

prop_classifyStream_reconstructs_input :: Property
prop_classifyStream_reconstructs_input = property $ do
  lns <- forAll (Gen.list (Range.linear 0 8) genOpaqueSafeLine)
  let results = classifyStream StdOut lns
      recovered = map recoverBytes results
  recovered === lns
  where
    recoverBytes (ClassifiedOpaque o)     = opaqueRawBytes o
    recoverBytes (ClassifiedDiagnostic _) = ""

phase9Tests :: [TestTree]
phase9Tests =
  [ testProperty "messageDoc preserves fragment order" prop_messageDoc_preserves_order
  , testProperty "messageDoc preserves fragment count" prop_messageDoc_preserves_fragment_count
  , testProperty "GHC code never becomes a Tadka code" prop_code_projection_always_Nothing
  , testProperty "unsupported schema versions never silently decode" prop_unsupported_version_always_Left
  , testProperty "flag always present after injection" prop_flag_always_present_after_injection
  , testProperty "flag injection is idempotent" prop_injection_idempotent
  , testProperty "classifyStream drops no input line (non-panic, non-JSON subset)"
      prop_classifyStream_reconstructs_input

  , testCase "BuildRunner: a fake runner enables deterministic testing without a real toolchain" $ do
      let fakeResult = BuildResult CompilerSucceeded [(StdOut, validDiagLine)]
          fakeRunner = BuildRunner (\_ _ _ _ -> pure fakeResult)
      result <- execute fakeRunner Nothing Cabal "." []
      assertEqual "fake result returned unchanged" fakeResult result
  ]


--------------------------------------------------------------------------------
-- Shared span builder (pure; buildSpanIO wraps it for HUnit)
--------------------------------------------------------------------------------

buildSpan :: FilePath -> Integer -> Integer -> Integer -> Integer -> Either DecodeError GhcSpan
buildSpan file sl sc el ec = do
  l1 <- mkLine sl
  c1 <- mkColumn sc
  l2 <- mkLine el
  c2 <- mkColumn ec
  mkGhcSpan file l1 c1 l2 c2

--------------------------------------------------------------------------------
-- Phase 2.7: Strict vs ForwardCompatible (§25)
--------------------------------------------------------------------------------

decodingModeTests :: [TestTree]
decodingModeTests =
  [ testCase "Strict rejects unknown fields on every schema version, reporting the first key in sorted order" $
      mapM_
        (\v -> do
            bs <- BS.readFile ("test/fixtures/schema/" <> v <> "/unknown-fields.json")
            case decodeDiagnosticLineWith Strict bs of
              Left (DecodeUnknownField _ f) -> assertEqual ("field @" <> v) "anotherOne" f
              other -> assertFailure ("expected DecodeUnknownField for " <> v <> ", got " <> show other))
        ["1.0", "1.1", "1.2"]

  , testCase "ForwardCompatible accepts them and reports each key with its raw value, sorted by key" $ do
      bs <- BS.readFile "test/fixtures/schema/1.0/unknown-fields.json"
      case decodeDiagnosticLineWith ForwardCompatible bs of
        Right (_, ws) -> do
          assertEqual "names"  ["anotherOne", "extraField"] [k | UnknownField k _ <- ws]
          assertEqual "values" [Number 42, String "surprise"] [v | UnknownField _ v <- ws]
        Left e -> assertFailure (show e)

  , testCase "Strict accepts a fixture with no unknown fields, with no warnings" $ do
      bs <- BS.readFile "test/fixtures/schema/1.2/rendered.json"
      case decodeDiagnosticLineWith Strict bs of
        Right (_, ws) -> assertEqual "warnings" [] ws
        Left e        -> assertFailure (show e)

  , testCase "decodeDiagnosticLine keeps its forward-compatible contract" $ do
      bs <- BS.readFile "test/fixtures/schema/1.1/unknown-fields.json"
      either (assertFailure . show) (const (pure ())) (decodeDiagnosticLine bs)

  , testCase "an unsupported version fails in BOTH modes, never silently reinterpreted" $ do
      bs <- BS.readFile "test/fixtures/schema/unsupported/version-1.3.json"
      mapM_
        (\m -> case decodeDiagnosticLineWith m bs of
            Left (DecodeUnsupportedVersion _) -> pure ()
            other -> assertFailure ("expected DecodeUnsupportedVersion, got " <> show other))
        [Strict, ForwardCompatible]
  ]

--------------------------------------------------------------------------------
-- Golden rendering (§9.1 render/*, I-24, I-27)
--------------------------------------------------------------------------------

goldenText :: String -> FilePath -> Text -> TestTree
goldenText name path txt = goldenVsString name path (pure (BL.fromStrict (encodeUtf8 txt)))

-- | Renders through tadka's real narratable renderer (prose, plain Text
-- output), so an empty-message diagnostic is checked end-to-end rather
-- than only at the messageDoc level.
renderNarratableText :: Tadka.Diagnostic e => e -> Either String Text
renderNarratableText e =
  case Tadka.selectRenderer (Tadka.withTarget Tadka.TNarratable Tadka.defaultConfig) of
    Tadka.SomeRenderer r@(Tadka.Narratable _) -> Right (Tadka.render r e)
    Tadka.SomeRenderer _ -> Left "selectRenderer did not return the narratable renderer"

emptyMessageGolden :: String -> TestTree
emptyMessageGolden ver =
  goldenVsString
    ("empty-message diagnostic renders end-to-end (" <> ver <> ")")
    ("test/fixtures/render/empty-message-diagnostic/" <> ver <> ".golden")
    (do result <- decodeFixtureFull ("test/fixtures/schema/" <> ver <> "/empty-message.json")
        case result of
          Left e  -> assertFailure (show e)
          Right d -> case renderNarratableText d of
            Left msg -> assertFailure msg
            Right t  -> pure (BL.fromStrict (encodeUtf8 t)))

goldenTests :: [TestTree]
goldenTests =
  [ goldenText "messageDoc: one fragment"   "test/fixtures/render/message/one-fragment.golden"
      (renderDocText (messageDoc ["Variable not in scope: a"]))
  , goldenText "messageDoc: multi fragment" "test/fixtures/render/message/multi-fragment.golden"
      (renderDocText (messageDoc ["first", "second", "third"]))
  , goldenText "messageDoc: empty"          "test/fixtures/render/message/empty.golden"
      (renderDocText (messageDoc []))
  , goldenText "helpDoc: one hint"          "test/fixtures/render/help/one-hint.golden"
      (maybe "<none>" renderDocText (helpDoc ["Add a type signature"]))
  , goldenText "helpDoc: multi hint"        "test/fixtures/render/help/multi-hint.golden"
      (maybe "<none>" renderDocText (helpDoc ["first hint", "second hint"]))
  , goldenText "helpDoc: empty is absence, not an empty block" "test/fixtures/render/help/empty.golden"
      (maybe "<none>" renderDocText (helpDoc []))
  , emptyMessageGolden "1.0"
  , emptyMessageGolden "1.1"
  , emptyMessageGolden "1.2"
  , testCase "an empty-message diagnostic bound to a real span still renders severity AND location" $ do
      result <- decodeFixtureFull "test/fixtures/schema/1.0/empty-message.json"
      d   <- either (assertFailure . show) pure result
      sp  <- buildSpanIO "Foo.hs" 1 5 1 6
      ctx <- either (assertFailure . show) pure (convertSpan (SourceText "let x = 1\n") sp)
      case renderNarratableText (BoundGhcDiagnostic d (SpanBound ctx)) of
        Left msg -> assertFailure msg
        Right t  -> do
          assertBool "severity present" ("Warning," `Text.isInfixOf` t)
          assertBool "location present" ("Foo.hs" `Text.isInfixOf` t)
  ]

--------------------------------------------------------------------------------
-- File-driven coordinate fixtures (§9.1, §31.2)
--------------------------------------------------------------------------------

coordinateFixtureNames :: [String]
coordinateFixtureNames =
  [ "first-char", "same-line", "multi-line", "eol", "final-char", "empty-span"
  , "unicode", "crlf", "invalid", "out-of-range"
  , "tab-real-baseline", "tab-real-col1", "tab-real-col5", "tab-real-col7"
  , "tab-real-col9", "tab-real-multibyte" ]

coordinateFixtureTest :: String -> TestTree
coordinateFixtureTest name = testCase ("coordinate fixture: " <> name) $ do
  let dir = "test/fixtures/coordinate/" <> name
  srcBytes      <- BS.readFile (dir <> "/source.txt")
  spanBytes     <- BS.readFile (dir <> "/span.json")
  expectedBytes <- BS.readFile (dir <> "/expected.txt")
  rawSpan <- either (assertFailure . ("bad span.json: " <>)) pure (eitherDecodeStrict spanBytes)
  let src      = decodeUtf8Lenient srcBytes
      expected = Text.strip (decodeUtf8Lenient expectedBytes)
      meta     = computeLineMetadata src
      showOff  = Text.pack . show :: Int -> Text
  case promoteSpan rawSpan of
    Left _   -> assertEqual "offsets" expected "ERR"
    Right gs ->
      case ( coordinateToOffset meta (spanStartLine gs) (spanStartCol gs)
           , coordinateToOffset meta (spanEndLine gs)   (spanEndCol gs) ) of
        (Right s, Right e) -> do
          assertEqual "offsets" expected ("OK " <> showOff s <> " " <> showOff e)
          either (assertFailure . show) (const (pure ())) (convertSpan (SourceText src) gs)
        _ -> do
          assertEqual "offsets" expected "ERR"
          case convertSpan (SourceText src) gs of
            Left _  -> pure ()
            Right _ -> assertFailure "convertSpan accepted coordinates coordinateToOffset rejected"

--------------------------------------------------------------------------------
-- Additional properties: Phase 3, 6, 8 definitions of done, and I-28
--------------------------------------------------------------------------------

genSpan :: Gen GhcSpan
genSpan = do
  file <- Gen.element ["Foo.hs", "src/Bar.hs", "<interactive>"]
  sl <- Gen.integral (Range.linear 1 4)
  sc <- Gen.integral (Range.linear 1 6)
  el <- Gen.integral (Range.linear sl 4)
  ec <- Gen.integral (Range.linear 1 6)
  case buildSpan file sl sc el ec of
    Right s -> pure s
    Left _  -> Gen.discard

genLookup :: Gen (Either SourceLookupError (Maybe SourceText))
genLookup = Gen.choice
  [ Left . SourceIOError <$> genSafeText
  , pure (Right Nothing)
  , Right . Just . SourceText . Text.intercalate "\n"
      <$> Gen.list (Range.linear 1 4) (Gen.text (Range.linear 0 6) Gen.alpha)
  ]

stateMatches :: Maybe GhcSpan -> Either SourceLookupError (Maybe SourceText) -> SpanState -> Bool
stateMatches Nothing  _                 NoSpan                       = True
stateMatches (Just _) (Left e)          (SpanSourceUnavailable _ e') = e == e'
stateMatches (Just _) (Right Nothing)   (SpanNoSource _)             = True
stateMatches (Just _) (Right (Just _))  (SpanBound _)                = True
stateMatches (Just _) (Right (Just _))  (SpanInvalidCoordinates _ _) = True
stateMatches _        _                 _                            = False

prop_bindSpan_total_and_classifies :: Property
prop_bindSpan_total_and_classifies = property $ do
  mSpan   <- forAll (Gen.maybe genSpan)
  outcome <- forAll genLookup
  let st = runIdentity (bindSpan (SourceProvider (\_ -> pure outcome)) mSpan)
  stateMatches mSpan outcome st === True

prop_bindSpan_uses_only_provided_provider :: Property
prop_bindSpan_uses_only_provided_provider = property $ do
  sp <- forAll genSpan
  let provider = SourceProvider (\p -> pure (Left (SourceIOError (Text.pack p))))
  case runIdentity (bindSpan provider (Just sp)) of
    SpanSourceUnavailable _ (SourceIOError t) -> t === Text.pack (spanFile sp)
    _ -> failure

prop_crlf_matches_lf_line_content :: Property
prop_crlf_matches_lf_line_content = property $ do
  ls <- forAll (Gen.list (Range.linear 1 6) (Gen.text (Range.linear 0 8) Gen.alpha))
  let lf   = computeLineMetadata (Text.intercalate "\n" ls)
      crlf = computeLineMetadata (Text.intercalate "\r\n" ls)
  map lineContent (toList crlf) === map lineContent (toList lf)

prop_feedChunk_split_invariant :: Property
prop_feedChunk_split_invariant = property $ do
  s <- forAll (BSC.pack <$> Gen.list (Range.linear 0 30) (Gen.element ['a', 'b', '\n']))
  i <- forAll (Gen.int (Range.linear 0 (BS.length s)))
  let (a, b)    = BS.splitAt i s
      (st1, l1) = feedChunk emptyFramerState a
      (st2, l2) = feedChunk st1 b
      (stW, lW) = feedChunk emptyFramerState s
  (l1 <> l2 <> flushFramer st2) === (lW <> flushFramer stW)

prop_stream_length_matches_nonblank :: Property
prop_stream_length_matches_nonblank = property $ do
  lns <- forAll (Gen.list (Range.linear 0 10) (Gen.element [validDiagLine, "", "garbage"]))
  length (decodeDiagnosticStream lns) === length (filter (not . BS.null) lns)

prop_ghcCode_preserved_through_projection :: Property
prop_ghcCode_preserved_through_projection = property $ do
  n <- forAll (Gen.integral (Range.linear 0 (999999 :: Integer)))
  let line = encodeUtf8
        ( "{\"version\":\"1.0\",\"ghcVersion\":\"9.10.1\",\"span\":null,\"severity\":\"Warning\",\"code\":"
        <> Text.pack (show n) <> ",\"message\":[],\"hints\":[]}" )
  case (decodeDiagnosticLine line, mkGhcDiagnosticCode n) of
    (Right d, Right c) -> do
      ghcCode d === Just c       -- never lost from GhcDiagnostic
      Tadka.code d === Nothing   -- never fabricated into a Tadka code
    _ -> failure

extraPropertyTests :: [TestTree]
extraPropertyTests =
  [ testProperty "bindSpan is total and classifies every input" prop_bindSpan_total_and_classifies
  , testProperty "bindSpan uses only the provider it is given, with the span's own path"
      prop_bindSpan_uses_only_provided_provider
  , testProperty "CRLF and LF sources have identical per-line content lengths"
      prop_crlf_matches_lf_line_content
  , testProperty "feedChunk: any split point reconstructs the same lines" prop_feedChunk_split_invariant
  , testProperty "decodeDiagnosticStream yields exactly one result per non-blank line"
      prop_stream_length_matches_nonblank
  , testProperty "GHC code stays on GhcDiagnostic while Tadka.code stays Nothing (I-28)"
      prop_ghcCode_preserved_through_projection
  , testCase "computeLineMetadata handles a 100k-line source (linear, not quadratic)" $
      assertEqual "line count" 100001 (length (computeLineMetadata (Text.replicate 100000 "ab\n")))
  ]

--------------------------------------------------------------------------------
-- Real subprocess integration (spec §9.4). The group name "real-subprocess"
-- is what ci.yml's --pattern filters on. Needs cabal + GHC on PATH.
--------------------------------------------------------------------------------

realSubprocessTests :: [TestTree]
realSubprocessTests =
  [ testCase "runBuild + classifyBuildOutput against a real cabal/GHC build with a type error" $ do
      tmp <- getTemporaryDirectory
      let dir = tmp </> "tadka-ghc-real-subprocess-scratch"
      removePathForcibly dir
      createDirectoryIfMissing True dir
      writeFile (dir </> "scratch.cabal") $ unlines
        [ "cabal-version: 3.0"
        , "name: scratch"
        , "version: 0.1.0.0"
        , "build-type: Simple"
        , ""
        , "executable scratch"
        , "  main-is: Main.hs"
        , "  build-depends: base"
        , "  default-language: Haskell2010"
        ]
      writeFile (dir </> "cabal.project") "packages: .\n"
      writeFile (dir </> "Main.hs") $ unlines
        [ "module Main (main) where"
        , "main :: IO ()"
        , "main = putStrLn (1 :: Int)"   -- deliberate type error
        ]
      timeout600 <- either (\e -> assertFailure ("mkTimeout 600: " <> show e)) pure (mkTimeout 600)
      result <- runBuild (Just timeout600) Cabal dir (injectDiagnosticsFlag Cabal [])
      let outcome = classifyBuildOutput result
          diags   = [d | ClassifiedDiagnostic d <- buildClassifications outcome]
          opaque  = [opaqueText o | ClassifiedOpaque o <- buildClassifications outcome]
      case buildCompilerResult outcome of
        CompilerFailed _ -> pure ()
        other -> assertFailure ("expected CompilerFailed, got " <> show other)
      assertBool
        ("expected at least one decoded GHC diagnostic; opaque records were: " <> show opaque)
        (not (null diags))
      assertBool "expected an error-severity diagnostic" (any ((== SevError) . ghcSeverity) diags)
  ]
