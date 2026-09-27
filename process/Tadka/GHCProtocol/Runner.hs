-- | Subprocess execution (vision §3; spec Phase 6.4), the sole function
-- in this package that performs process I/O. Lives in the separate
-- 'tadka-ghc-process' component (I-35): the core library's detection
-- and flag injection ('Tadka.GHCProtocol.BuildTool') have no dependency
-- on 'typed-process' at all.
module Tadka.GHCProtocol.Runner
  ( runBuild
  , BuildRunner (..)
  , ioBuildRunner
  ) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (IOException, finally, try)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime)
import System.Exit (ExitCode (..))
import System.IO (Handle)
import qualified System.Timeout
import System.Process.Typed
  ( Process
  , closed
  , createPipe
  , getStderr
  , getStdout
  , proc
  , setStderr
  , setStdin
  , setStdout
  , setWorkingDir
  , startProcess
  , stopProcess
  , waitExitCode
  )

import Tadka.GHCProtocol.BuildTool
  (BuildTool (..), FramerState, emptyFramerState, feedChunk, flushFramer)
import Tadka.GHCProtocol.Process
  (BuildResult (..), CompilerResult (..), OutputStream (..), ProcessError (..), Signal (..))

buildToolExecutable :: BuildTool -> String
buildToolExecutable Cabal = "cabal"
buildToolExecutable Stack = "stack"

-- | Best-effort, platform-dependent heuristic (flagged explicitly, not
-- silently assumed correct): on POSIX, a process killed by signal S is
-- conventionally reported by process-management libraries as exit code
-- -S. This is not guaranteed by any standard; on Windows there are no
-- signals, so this branch simply never triggers there. External
-- verification required per-platform before relying on this in a
-- release build.
resultFromExitCode :: ExitCode -> CompilerResult
resultFromExitCode ExitSuccess = CompilerSucceeded
resultFromExitCode (ExitFailure n)
  | n < 0     = CompilerSignalled (Signal (negate n))
  | otherwise = CompilerFailed (ExitFailure n)

-- | Appends one completed line to the single shared, thread-safe output
-- buffer both reader threads push into, preserving the order each
-- thread observed its own chunks arriving in -- this is what gives the
-- overall result its "observed order" guarantee, not a claim about the
-- child's true cross-stream write order (I-19).
appendLine :: TVar [(OutputStream, BS.ByteString)] -> OutputStream -> BS.ByteString -> IO ()
appendLine var stream line = atomically (modifyTVar' var ((stream, line) :))

-- | Total: reads chunks from one pipe until real EOF ('hGetSome'
-- returning empty), feeding each into that stream's own 'FramerState'
-- (never shared with the other stream, per §6.4's independence
-- requirement) and appending every completed line to the shared
-- accumulator as it's observed.
drainLoop :: OutputStream -> Handle -> IORef FramerState -> TVar [(OutputStream, BS.ByteString)] -> IO ()
drainLoop stream h framerRef accumVar = loop
  where
    loop = do
      chunk <- BS.hGetSome h 4096
      if BS.null chunk
        then do
          framer <- readIORef framerRef
          mapM_ (appendLine accumVar stream) (flushFramer framer)
        else do
          framer <- readIORef framerRef
          let (framer', completed) = feedChunk framer chunk
          writeIORef framerRef framer'
          mapM_ (appendLine accumVar stream) completed
          loop

-- | The full spawn/drain/timeout/terminate/cleanup lifecycle (spec
-- Phase 6.4, steps 1-7). Both stdout and stderr are configured as pipes
-- the parent reads -- never inherited, so nothing the child prints can
-- leak past this function uncaptured. A start failure (executable not
-- found, permission denied, missing working directory) is caught and
-- reported as 'CompilerStartFailed'; nothing past that point runs and
-- no partially-spawned process is left behind. Once started, the child
-- process is always terminated (via 'finally' wrapping the whole
-- lifecycle) whether this function returns normally, times out, or is
-- itself the target of an exception -- no file descriptor and no child
-- process is ever leaked.
runBuild :: Maybe NominalDiffTime -> BuildTool -> FilePath -> [Text] -> IO BuildResult
runBuild mTimeout tool workDir extraArgs = do
  accumVar     <- newTVarIO []
  outFramerRef <- newIORef emptyFramerState
  errFramerRef <- newIORef emptyFramerState
  let exe  = buildToolExecutable tool
      args = "build" : map Text.unpack extraArgs
      pc   = setStdin closed
           $ setStdout createPipe
           $ setStderr createPipe
           $ setWorkingDir workDir
           $ proc exe args

  spawnAttempt <- try (startProcess pc) :: IO (Either IOException (Process () Handle Handle))
  case spawnAttempt of
    Left ioErr ->
      pure (BuildResult (CompilerStartFailed (ProcessError (Text.pack (show ioErr)))) [])
    Right p -> do
      compResult <-
        runLifecycle mTimeout p outFramerRef errFramerRef accumVar
          `finally` stopProcess p
          -- stopProcess is called again here even if runLifecycle
          -- already called it on a timeout path below -- idempotent by
          -- design, and this is what guarantees cleanup on every other
          -- exception path too (step 6 of the spec's lifecycle).
      observed <- reverse <$> readTVarIO accumVar
      pure (BuildResult compResult observed)

runLifecycle
  :: Maybe NominalDiffTime
  -> Process () Handle Handle
  -> IORef FramerState
  -> IORef FramerState
  -> TVar [(OutputStream, BS.ByteString)]
  -> IO CompilerResult
runLifecycle mTimeout p outFramerRef errFramerRef accumVar = do
  outDone <- newEmptyMVar
  errDone <- newEmptyMVar
  _ <- forkFinally (drainLoop StdOut (getStdout p) outFramerRef accumVar) (\_ -> putMVar outDone ())
  _ <- forkFinally (drainLoop StdErr (getStderr p) errFramerRef accumVar) (\_ -> putMVar errDone ())

  mExitCode <- case mTimeout of
    Nothing  -> Just <$> waitExitCode p
    Just dur -> System.Timeout.timeout (round (dur * 1000000) :: Int) (waitExitCode p)

  compResult <- case mExitCode of
    Just ec -> pure (resultFromExitCode ec)
    Nothing -> do
      -- Timed out: terminate, then keep draining until both pipes
      -- independently report EOF (below) rather than discarding
      -- whatever the process had already written.
      stopProcess p
      pure CompilerTimedOut

  -- Draining continues until both reader threads have independently
  -- observed EOF -- a process can exit while its pipes still hold
  -- buffered, unread data.
  takeMVar outDone
  takeMVar errDone
  pure compResult

--------------------------------------------------------------------------------
-- Deterministic testing capability (spec Phase 9.4): a fake 'BuildRunner'
-- lets the full test suite run without a real cabal/stack/GHC toolchain.
--------------------------------------------------------------------------------

newtype BuildRunner m = BuildRunner
  { execute :: Maybe NominalDiffTime -> BuildTool -> FilePath -> [Text] -> m BuildResult }

ioBuildRunner :: BuildRunner IO
ioBuildRunner = BuildRunner runBuild
