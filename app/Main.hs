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

import qualified Data.Text as Text
import qualified System.FilePath as FP
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import qualified Tadka

import Tadka.GHCProtocol
import Tadka.GHCProtocol.Cli (CliOptions (..), getCliOptions)
import Tadka.GHCProtocol.Runner (BuildRunner (..), ioBuildRunner)

--------------------------------------------------------------------------------
-- Source binding: resolve a GHC-reported path relative to the project
-- directory being built, not this process's own working directory.
--------------------------------------------------------------------------------

projectSourceProvider :: FilePath -> SourceProvider IO
projectSourceProvider dir = SourceProvider $ \path ->
  lookupSource fileSourceProvider (if FP.isAbsolute path then path else dir FP.</> path)

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

reportClassification :: Tadka.Config -> SourceProvider IO -> LineClassification -> IO ()
reportClassification cfg provider (ClassifiedDiagnostic d) = do
  spanState <- bindSpan provider (ghcSpan d)
  Tadka.reportDiagnostic cfg (BoundGhcDiagnostic d spanState)
reportClassification cfg _ (ClassifiedOpaque o) =
  Tadka.reportDiagnostic cfg o

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
          provider = projectSourceProvider (cliDir opts)
          toShow   = filter (shouldReport (cliVerbose opts) (buildCompilerResult outcome))
                            (buildClassifications outcome)
      mapM_ (reportClassification cfg provider) toShow
      exitWith (exitCodeFor (buildCompilerResult outcome))

main :: IO ()
main = getCliOptions >>= runTadkaGhc
