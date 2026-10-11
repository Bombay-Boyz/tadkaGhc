-- | The runner against REAL processes (audit B1, B2, B3): actual @sh@
-- scripts, actual pipes, process groups and signals. Nothing here is
-- faked, which is the point: these are the situations a fake cannot
-- reproduce.
--
-- Process groups are a POSIX feature, so the tests that depend on them
-- are skipped on Windows (@os == "mingw32"@).
module RunnerTests
  ( runnerTests
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString.Char8 as BS8
import Data.List (isInfixOf)
import GHC.Clock (getMonotonicTime)
import System.Directory
  ( createDirectory
  , doesFileExist
  , findExecutable
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Info (os)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import Tadka.GHCProtocol
import Tadka.GHCProtocol.Runner (RunSettings (..), runCaptured)

-- | Short grace periods, so the tests that exercise them are quick.
fast :: RunSettings
fast = RunSettings { terminateGraceMicros = 400000, drainGraceMicros = 400000 }

timeoutOf :: Double -> IO Timeout
timeoutOf s = either (assertFailure . show) pure (mkTimeout s)

-- | Run a shell script as the build; returns the result and the seconds taken.
runSh :: Maybe Timeout -> String -> IO (BuildResult, Double)
runSh mt script = do
  t0 <- getMonotonicTime
  r  <- runCaptured fast mt "." "sh" ["-c", script]
  t1 <- getMonotonicTime
  pure (r, t1 - t0)

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir act = do
  tmp <- getTemporaryDirectory
  (path, h) <- openTempFile tmp "tadka-ghc-run"
  hClose h
  removeFile path
  createDirectory path
  act path `finally` removeDirectoryRecursive path

linesOf :: OutputStream -> BuildResult -> [BS8.ByteString]
linesOf s r = [l | (s', l) <- observedOutput r, s' == s]

onPosix :: IO () -> IO ()
onPosix = unless (os == "mingw32")

-- | After the build has been stopped, wait long enough for a leftover
-- process (which would write its marker after one second) to have done so.
settle :: IO ()
settle = threadDelay 1400000

runnerTests :: [TestTree]
runnerTests =
  [ testCase "about 2 MB on stderr is captured completely, with no hang (B1)" $ do
      (r, secs) <- runSh Nothing "seq 1 300000 >&2"
      compilerResult r @?= CompilerSucceeded
      captureStatus r @?= CaptureComplete
      length (linesOf StdErr r) @?= 300000
      last' (linesOf StdErr r) @?= Just "300000"
      assertBool ("took " <> show secs <> " s") (secs < 20)

  , testCase "large output on both streams at the same time is captured completely (B1)" $ do
      (r, _) <- runSh Nothing "seq 1 200000 >&2 & seq 1 200000; wait"
      captureStatus r @?= CaptureComplete
      length (linesOf StdErr r) @?= 200000
      length (linesOf StdOut r) @?= 200000

  , testCase "a final line without a newline is kept" $ do
      (r, _) <- runSh Nothing "printf 'one\\ntwo' >&2"
      linesOf StdErr r @?= ["one", "two"]
      captureStatus r @?= CaptureComplete

  , testCase "exit statuses and signals are reported as what they are" $ do
      (r1, _) <- runSh Nothing "exit 3"
      compilerResult r1 @?= CompilerFailed (ExitFailure 3)
      onPosix $ do
        (r2, _) <- runSh Nothing "kill -9 $$"
        compilerResult r2 @?= CompilerSignalled (Signal 9)

  , testCase "a missing executable is a start failure, not a crash" $ do
      r <- runCaptured fast Nothing "." "tadka-ghc-no-such-tool" []
      case compilerResult r of
        CompilerStartFailed _ -> captureStatus r @?= CaptureComplete
        other                 -> assertFailure ("expected CompilerStartFailed, got " <> show other)

  , testCase "a missing working directory is a start failure, not a crash" $ do
      r <- runCaptured fast Nothing "/no/such/directory/tadka" "sh" ["-c", "true"]
      case compilerResult r of
        CompilerStartFailed _ -> pure ()
        other                 -> assertFailure ("expected CompilerStartFailed, got " <> show other)

  , testCase "a timeout stops the build promptly and takes its child processes with it (B2)" $
      onPosix $ withTempDir $ \dir -> do
        let marker = dir </> "survived"
        t <- timeoutOf 0.4
        -- like cabal starting ghc: a child that keeps the output pipes open
        -- and would write a marker after one second if it were still alive
        (r, secs) <- runSh (Just t) ("(sleep 1; echo x > " <> marker <> ") & echo started >&2; wait")
        compilerResult r @?= CompilerTimedOut
        assertBool ("returned after " <> show secs <> " s; a 0.4 s timeout must not wait for the child") (secs < 3)
        assertBool "the output before the timeout was kept" ("started" `elem` linesOf StdErr r)
        settle
        survived <- doesFileExist marker
        assertBool "the child process outlived the timeout" (not survived)

  , testCase "a build that ignores SIGTERM is killed once the grace period ends (B2)" $
      onPosix $ withTempDir $ \dir -> do
        let marker = dir </> "survived"
        t <- timeoutOf 0.3
        (r, secs) <- runSh (Just t)
          ("trap '' TERM; (trap '' TERM; sleep 1; echo x > " <> marker <> ") & echo started >&2; wait")
        compilerResult r @?= CompilerTimedOut
        assertBool ("returned after " <> show secs <> " s") (secs < 4)
        settle
        survived <- doesFileExist marker
        assertBool "a SIGTERM-ignoring process was not killed" (not survived)

  , testCase "a process that escapes the group and holds the output open does not stall the capture (bounded drain)" $
      onPosix $ do
        mSetsid <- findExecutable "setsid"
        case mSetsid of
          Nothing -> pure ()   -- no setsid on this platform: nothing to escape with
          Just _  -> do
            -- the build tool exits at once; a process in ITS OWN SESSION keeps stderr open for 2 s
            (r, secs) <- runSh Nothing "printf 'complete\\npartial' >&2; setsid sleep 2 & exit 0"
            compilerResult r @?= CompilerSucceeded
            captureStatus r @?= CaptureStoppedEarly
            assertBool ("waited " <> show secs <> " s for a 0.4 s drain grace") (secs < 1.8)
            -- the half-written last line is kept even though reading was cut off (B3)
            linesOf StdErr r @?= ["complete", "partial"]

  , testCase "an exception in the calling thread stops the build's whole process group (no leaked processes)" $
      onPosix $ withTempDir $ \dir -> do
        let marker  = dir </> "survived"
            started = dir </> "started"
        done <- newEmptyMVar
        tid  <- forkIO $
          void (runCaptured fast Nothing "." "sh"
                  ["-c", "echo up > " <> started <> "; (sleep 1; echo x > " <> marker <> ") & wait"])
            `finally` putMVar done ()
        -- wait for the build to be running, then interrupt the caller
        let waitUp n = do
              up <- doesFileExist started
              when (not up && n > (0 :: Int)) (threadDelay 50000 >> waitUp (n - 1))
        waitUp 60
        up <- doesFileExist started
        assertBool "the fake build never started" up
        killThread tid
        takeMVar done
        settle
        survived <- doesFileExist marker
        assertBool "the build's process group was left running after the caller was interrupted" (not survived)

  , testCase "an incomplete capture is described in plain English; a complete one is not mentioned" $ do
      renderCaptureStatus CaptureComplete @?= Nothing
      case renderCaptureStatus CaptureStoppedEarly of
        Just t  -> assertBool (show t) ("stopped reading" `isInfixOf` show t)
        Nothing -> assertFailure "expected a note for CaptureStoppedEarly"
      case renderCaptureStatus (CaptureReadFailed "boom") of
        Just t  -> assertBool (show t) ("boom" `isInfixOf` show t)
        Nothing -> assertFailure "expected a note for CaptureReadFailed"

  , testCase "classification passes the capture status through" $
      forM_ [CaptureComplete, CaptureStoppedEarly, CaptureReadFailed "x"] $ \st ->
        buildCaptureStatus (classifyBuildOutput (BuildResult CompilerSucceeded [] st)) @?= st
  ]
  where
    last' xs = case reverse xs of
      (x : _) -> Just x
      []      -> Nothing
