-- | Operator-facing input validation (audit B13) and messages (audit
-- B14): the typed build timeout, the command-line parser, and the
-- renderers for errors that reach a terminal.
module CliTests
  ( timeoutTests
  , cliTests
  ) where

import Data.List (isInfixOf, isPrefixOf)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as Text
import Hedgehog (assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Options.Applicative as Opt
import System.Exit (ExitCode (..))
import qualified Tadka
import Test.Tasty (TestTree)
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog (testProperty)

import Tadka.GHCProtocol
import Tadka.GHCProtocol.Cli

--------------------------------------------------------------------------------
-- mkTimeout
--------------------------------------------------------------------------------

micros :: Double -> Either TimeoutError Int
micros = fmap timeoutMicroseconds . mkTimeout

timeoutTests :: [TestTree]
timeoutTests =
  [ testCase "whole and fractional seconds convert exactly to microseconds" $ do
      micros 30  @?= Right 30000000
      micros 2.5 @?= Right 2500000

  , testCase "a tiny positive value rounds UP to 1 microsecond, never down to 0" $ do
      micros 1e-9   @?= Right 1
      micros 5e-324 @?= Right 1

  , testCase "the one-year limit is inclusive; anything above it is rejected" $ do
      micros maxTimeoutSeconds         @?= Right 31536000000000
      micros (maxTimeoutSeconds + 1)   @?= Left TimeoutTooLarge

  , testCase "zero and negative values are rejected (a negative value used to mean 'no timeout')" $ do
      micros 0      @?= Left TimeoutNotPositive
      micros (-1)   @?= Left TimeoutNotPositive
      micros (-0.0) @?= Left TimeoutNotPositive

  , testCase "NaN and the infinities are rejected" $ do
      micros (0 / 0)    @?= Left TimeoutNotFinite
      micros (1 / 0)    @?= Left TimeoutNotFinite
      micros (-1 / 0)   @?= Left TimeoutNotFinite

  , testCase "an absurdly large value is rejected rather than overflowing" $
      micros 1e30 @?= Left TimeoutTooLarge

  , testProperty "every value in (0, one year] is accepted and rounded up to a whole microsecond" $
      property $ do
        s <- forAll (Gen.double (Range.exponentialFloat 1e-9 maxTimeoutSeconds))
        case mkTimeout s of
          Left _  -> assert False
          Right t -> do
            let us = timeoutMicroseconds t
            assert (us >= 1)
            assert (fromIntegral us >= s * 1000000)
            assert (fromIntegral us < s * 1000000 + 1)

  , testProperty "every non-positive value is rejected" $
      property $ do
        s <- forAll (Gen.double (Range.linearFrac (-1e9) 0))
        micros s === Left TimeoutNotPositive

  , testProperty "every value above one year is rejected" $
      property $ do
        s <- forAll (Gen.double (Range.exponentialFloat (maxTimeoutSeconds + 1) 1e300))
        micros s === Left TimeoutTooLarge

  , testCase "timeout errors read as plain English" $ do
      renderTimeoutError TimeoutNotFinite   @?= "must be a finite number"
      renderTimeoutError TimeoutNotPositive @?= "must be greater than 0"
      renderTimeoutError TimeoutTooLarge    @?= "must be at most 31536000 seconds (one year)"
  ]

--------------------------------------------------------------------------------
-- Command line
--------------------------------------------------------------------------------

parsed :: [String] -> IO CliOptions
parsed args = case parseCliArgs args of
  Opt.Success o           -> pure o
  Opt.Failure f           -> assertFailure
    ("expected success, got: " <> fst (Opt.renderFailure f "tadka-ghc"))
  Opt.CompletionInvoked _ -> assertFailure "unexpected completion request"

-- | The rendered message and exit code of a parse that did not produce
-- options (usage error, @--help@ or @--version@).
stopped :: [String] -> IO (String, ExitCode)
stopped args = case parseCliArgs args of
  Opt.Failure f           -> pure (Opt.renderFailure f "tadka-ghc")
  Opt.Success o           -> assertFailure ("expected a failure, got " <> show o)
  Opt.CompletionInvoked _ -> assertFailure "unexpected completion request"

cliTests :: [TestTree]
cliTests =
  [ testCase "no arguments gives every default" $ do
      o <- parsed []
      o @?= CliOptions
        { cliDir = "."
        , cliTool = Nothing
        , cliTarget = Nothing
        , cliTimeout = Nothing
        , cliVerbose = False
        , cliExtraArgs = []
        }

  , testCase "every option together, with pass-through arguments" $ do
      o <- parsed ["proj", "--stack", "--json", "--timeout=2.5", "-v", "--", "--ghc-options=-O2", "pkg"]
      cliDir o @?= "proj"
      cliTool o @?= Just Stack
      cliTarget o @?= Just Tadka.TJson
      fmap timeoutMicroseconds (cliTimeout o) @?= Just 2500000
      cliVerbose o @?= True
      cliExtraArgs o @?= ["--ghc-options=-O2", "pkg"]

  , testCase "--timeout also accepts the separate-argument form" $ do
      o <- parsed ["--timeout", "3"]
      fmap timeoutMicroseconds (cliTimeout o) @?= Just 3000000

  , testCase "invalid --timeout values are usage errors naming the value (B13)" $
      mapM_
        (\bad -> do
            (msg, code) <- stopped ["--timeout=" <> bad]
            assertEqual ("exit status for " <> show bad) (ExitFailure 2) code
            assertBool ("message names " <> show bad <> ": " <> msg) (bad `isInfixOf` msg))
        ["-1", "0", "1e30", "NaN", "Infinity", "-Infinity", "abc"]

  , testCase "--help and -h print usage and succeed" $
      mapM_
        (\flag -> do
            (msg, code) <- stopped [flag]
            code @?= ExitSuccess
            assertBool ("usage text for " <> flag) ("Usage:" `isInfixOf` msg)
            assertBool "documents pass-through" ("Everything after --" `isInfixOf` msg))
        ["--help", "-h"]

  , testCase "--help and -h after -- belong to the build tool, not to this program (B13)" $ do
      o1 <- parsed ["--", "--help"]
      cliExtraArgs o1 @?= ["--help"]
      o2 <- parsed ["--", "-h", "--timeout=-1"]
      cliExtraArgs o2 @?= ["-h", "--timeout=-1"]
      cliTimeout o2 @?= Nothing

  , testCase "only the first -- separates; later ones are passed through" $ do
      o <- parsed ["--", "a", "--", "b"]
      cliExtraArgs o @?= ["a", "--", "b"]

  , testCase "--version names the program and succeeds" $ do
      (msg, code) <- stopped ["--version"]
      code @?= ExitSuccess
      assertBool msg ("tadka-ghc " `isPrefixOf` msg)

  , testCase "conflicting, unknown and surplus arguments are usage errors" $
      mapM_
        (\args -> do
            (_, code) <- stopped args
            assertEqual (unwords args) (ExitFailure 2) code)
        [ ["--cabal", "--stack"]
        , ["--graphical", "--json"]
        , ["--bogus"]
        , ["one", "two"]
        ]

  , testCase "build-tool detection errors read as plain English (B14)" $ do
      let none      = renderBuildToolDetectionError NoRecognizedProjectFile
          ambiguous = renderBuildToolDetectionError (AmbiguousProjectFiles ("a.cabal" :| ["b.cabal"]))
          unreadable = renderBuildToolDetectionError (ProjectDirectoryUnreadable "/x" "does not exist")
      assertBool "names the files" ("a.cabal" `Text.isInfixOf` ambiguous && "b.cabal" `Text.isInfixOf` ambiguous)
      assertBool "carries the reason" ("does not exist" `Text.isInfixOf` unreadable)
      mapM_
        (\t -> assertBool ("leaks a constructor name: " <> Text.unpack t)
                 (not (any (`Text.isInfixOf` t)
                         ["NoRecognizedProjectFile", "AmbiguousProjectFiles", "ProjectDirectoryUnreadable"])))
        [none, ambiguous, unreadable]
  ]
