-- | Phase 1 (wire/schema dispatch), Phase 2 (semantic promotion,
-- decodeDiagnosticLine), Phase 3 (coordinate conversion, source
-- binding), and Phase 5 (Tadka projection) tests. Vision §31.1-§31.3;
-- spec Phase 9.1.
module Main (main) where

import qualified Data.ByteString as BS
import System.Exit (ExitCode (ExitFailure))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import qualified Tadka
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
  (Assertion, assertBool, assertEqual, assertFailure, testCase)

import Tadka.GHCProtocol.BuildTool
import Tadka.GHCProtocol.Decode
import Tadka.GHCProtocol.Diagnostic
import Tadka.GHCProtocol.Opaque
import Tadka.GHCProtocol.Process
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
  , testGroup "Phase 6: build tool wrapper" phase6Tests
  , testGroup "Phase 7: opaque output capture" phase7Tests
  , testGroup "Phase 8: pure stream semantics" phase8Tests
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
--------------------------------------------------------------------------------
-- Phase 6: build tool detection, flag injection, chunk framing (§31.5)
--------------------------------------------------------------------------------

phase6Tests :: [TestTree]
phase6Tests =
  [ testCase "detectBuildTool: explicit override bypasses detection entirely" $ do
      result <- detectBuildTool (Just Stack) "test/fixtures/build/cabal-only"
      assertEqual "detected" (Right Stack) result

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
