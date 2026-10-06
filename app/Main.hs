-- | The tadka-ghc executable (vision §3; spec Phase 6's CLI entry
-- point). Wires together: build-tool detection, flag injection,
-- subprocess execution, classification of the captured output, and
-- Tadka's own rendering.
--
-- Decoded diagnostics are bound against real source files on disk
-- (fileSourceProvider + bindSpan) before being reported, so the
-- graphical renderer can show the offending source line -- reporting
-- the bare GhcDiagnostic would always show NoContext, per Phase 5's
-- own design.
--
-- Default vs --verbose output (a presentation-layer decision, NOT a
-- change to the underlying data model -- BuildOutcome/classifyBuildOutput
-- remain exactly as tested; nothing here discards anything, it only
-- decides what to print by default):
--   * Every decoded GhcDiagnostic is always shown, regardless of the
--     overall build outcome -- a warning on an otherwise-successful
--     build is real signal and must not be hidden.
--   * OpaqueGhcOutput's own Tadka.Diagnostic instance defaults its
--     severity to SevError unconditionally (vision §4: "so nothing is
--     ever silently dropped") -- deliberately, since arbitrary captured
--     text carries no genuine severity and understating a possible
--     Setup.hs crash or OOM kill would be worse than overstating
--     ordinary progress noise. That is a data-model decision this CLI
--     does not second-guess. What this CLI DOES decide is when to
--     print those opaque records: by default, only when the build did
--     NOT succeed -- on success they are categorically just cabal's own
--     progress chatter, and on failure the preceding lines are often
--     genuine debugging context (which package's Setup.hs was running,
--     etc.), mirroring how cabal/stack themselves behave (quiet on
--     success, full transcript on failure). --verbose always shows
--     every captured record, matching the vision document's own
--     literal behaviour exactly.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import qualified Tadka

import Tadka.GHCProtocol
import Tadka.GHCProtocol.Cli (CliOptions (..), getCliOptions)
import Tadka.GHCProtocol.Runner (BuildRunner (..), ioBuildRunner)

--------------------------------------------------------------------------------
-- Source binding: GHC-reported paths are resolved across the project's
-- package roots by 'projectSourceProvider' (library), never by joining
-- them onto the project directory alone, and an ambiguous path is
-- refused rather than guessed (audit D3).
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Reporting
--------------------------------------------------------------------------------

-- | Total, one equation per LineClassification constructor: a decoded
-- diagnostic is always shown; an opaque record is shown only in
-- --verbose mode or when the build did not succeed. See the module
-- Haddock for the reasoning -- this is presentation policy only, never
-- a change to what BuildOutcome itself contains.
shouldReport :: Bool -> CompilerResult -> LineClassification -> Bool
shouldReport _       _      (ClassifiedDiagnostic _) = True
shouldReport verbose result (ClassifiedOpaque _)      = verbose || result /= CompilerSucceeded

-- | A diagnostic is always shown. When it has a location but no source
-- excerpt, one note on stderr says why (never silently omitted). Notes go
-- to stderr, so a machine-readable stdout (--json) is never polluted, and
-- each distinct note is printed once per run, not once per diagnostic.
reportClassification
  :: Tadka.Config -> SourceProvider IO -> IORef [Text.Text] -> LineClassification -> IO ()
reportClassification cfg provider notesSeen (ClassifiedDiagnostic d) = do
  spanState <- bindSpan provider (ghcSpan d)
  Tadka.reportDiagnostic cfg (BoundGhcDiagnostic d spanState)
  forM_ (renderSpanStateNote spanState) (noteOnce notesSeen)
reportClassification cfg _ _ (ClassifiedOpaque o) =
  Tadka.reportDiagnostic cfg o

noteOnce :: IORef [Text.Text] -> Text.Text -> IO ()
noteOnce ref note = do
  seen <- readIORef ref
  unless (note `elem` seen) $ do
    writeIORef ref (note : seen)
    hPutStrLn stderr ("tadka-ghc: note: " <> Text.unpack note)

-- | Total: one equation per 'CompilerResult' constructor. Exit codes
-- follow common shell conventions where one exists (124 for timeout,
-- matching the "timeout" command; 127 for "command not found"-style
-- start failure; 130 for signalled termination, the conventional
-- 128+SIGINT region) rather than being invented arbitrarily. A normal
-- non-zero exit passes the real exit code straight through.
exitCodeFor :: CompilerResult -> ExitCode
exitCodeFor CompilerSucceeded       = ExitSuccess
exitCodeFor (CompilerFailed ec)     = ec
exitCodeFor (CompilerSignalled _)   = ExitFailure 130
exitCodeFor CompilerTimedOut        = ExitFailure 124
exitCodeFor (CompilerStartFailed _) = ExitFailure 127

runTadkaGhc :: CliOptions -> IO ()
runTadkaGhc opts = do
  -- Checked first: with --cabal/--stack there is no detection step to
  -- notice a missing directory, and the process library's own error for
  -- a bad working directory is cryptic.
  dirExists <- doesDirectoryExist (cliDir opts)
  unless dirExists $ do
    hPutStrLn stderr ("tadka-ghc: the project directory does not exist: " <> cliDir opts)
    exitWith (ExitFailure 2)
  toolResult <- detectBuildTool (cliTool opts) (cliDir opts)
  case toolResult of
    Left err -> do
      hPutStrLn stderr
        ("tadka-ghc: could not detect a build tool in " <> cliDir opts <> ": "
           <> Text.unpack (renderBuildToolDetectionError err))
      exitWith (ExitFailure 1)
    Right tool -> do
      let finalArgs = injectDiagnosticsFlag tool (cliExtraArgs opts)
      buildResult <- execute ioBuildRunner (cliTimeout opts) tool (cliDir opts) finalArgs
      let outcome  = classifyBuildOutput buildResult
          cfg      = maybe Tadka.defaultConfig (\t -> Tadka.withTarget t Tadka.defaultConfig) (cliTarget opts)
          toShow   = filter (shouldReport (cliVerbose opts) (buildCompilerResult outcome))
                            (buildClassifications outcome)
      provider  <- projectSourceProvider (cliDir opts)
      notesSeen <- newIORef []
      mapM_ (reportClassification cfg provider notesSeen) toShow
      exitWith (exitCodeFor (buildCompilerResult outcome))

main :: IO ()
main = getCliOptions >>= runTadkaGhc
