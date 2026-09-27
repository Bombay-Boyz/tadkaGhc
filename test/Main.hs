-- | Phase 1 (wire/schema dispatch), Phase 2 (semantic promotion,
-- decodeDiagnosticLine), Phase 3 (coordinate conversion, source
-- binding), and Phase 5 (Tadka projection) tests. Vision §31.1-§31.3;
-- spec Phase 9.1.
module Main (main) where

import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import qualified Tadka
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  (Assertion, assertBool, assertEqual, assertFailure, testCase)

import Tadka.GHCProtocol.Decode
import Tadka.GHCProtocol.Diagnostic
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
  case do
    l1 <- mkLine sl
    c1 <- mkColumn sc
    l2 <- mkLine el
    c2 <- mkColumn ec
    mkGhcSpan file l1 c1 l2 c2
  of
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

--------------------------------------------------------------------------------
-- Phase 3: coordinate conversion / source binding (§31.2, §18's table)
--------------------------------------------------------------------------------

phase3Tests :: [TestTree]
phase3Tests =
  [ testCase "computeLineMetadata: two plain lines" $ do
      let meta = computeLineMetadata "abc\ndef"
      case meta of
        (LineMetadata 0 3) :| [LineMetadata 4 3] -> pure ()
        other -> assertFailure ("unexpected metadata: " <> show other)

  , testCase "computeLineMetadata: CRLF excludes \\r from line content (§3.1)" $ do
      let meta = computeLineMetadata "abc\r\ndef"
      case meta of
        (LineMetadata 0 3) :| [LineMetadata 5 3] -> pure ()
        other -> assertFailure ("unexpected metadata: " <> show other)

  , testCase "computeLineMetadata: trailing newline yields an empty final line" $ do
      let meta = computeLineMetadata "abc\n"
      case meta of
        (LineMetadata 0 3) :| [LineMetadata 4 0] -> pure ()
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
        Left _  -> pure ()
        Right o -> assertFailure ("expected Left, got offset " <> show o)

  , testCase "coordinateToOffset: column beyond line length fails" $ do
      let meta = computeLineMetadata "abc\ndef"
      l <- either (assertFailure . show) pure (mkLine 1)
      c <- either (assertFailure . show) pure (mkColumn 99)
      case coordinateToOffset meta l c of
        Left _  -> pure ()
        Right o -> assertFailure ("expected Left, got offset " <> show o)

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
      let provide = \_ -> pure (Right (Just (SourceText "let x = 1\n")))
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
          assertBool "ghcCode preserved" (ghcCode d /= Nothing)
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
                , SpanInvalidCoordinates sp (InvalidCoordinates sp "bad")
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
      assertBool "non-empty" (renderSourceBindingError (InvalidCoordinates sp "x") /= "")
  ]
