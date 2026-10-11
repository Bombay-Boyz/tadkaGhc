{-# LANGUAGE CPP #-}
-- | Subprocess execution (vision §3; spec Phase 6.4), the sole function
-- in this package that performs process I/O. Lives in the separate
-- 'tadka-ghc-process' component (I-35): the core library's detection
-- and flag injection ('Tadka.GHCProtocol.BuildTool') have no dependency
-- on 'typed-process' at all.
--
-- == Guarantees
--
-- * __Output is never lost silently.__ Both output pipes are read
--   concurrently until their end. Whatever was received is kept, including
--   a final line with no newline, on every way a read can end: normally, by
--   an I/O error, or because reading was stopped. When the output is not
--   known to be complete, the result's 'captureStatus' says so.
--
-- * __A timeout is enforced.__ On POSIX systems the build tool runs in its
--   own process group. When the timeout expires, the whole group is asked to
--   stop (SIGTERM), given a short grace period, then killed (SIGKILL), so the
--   compilers a build starts die with it instead of outliving it.
--
-- * __Waiting for the end of the output is bounded.__ A process the build
--   started can hold an output pipe open long after the build tool exited.
--   The collector waits a bounded time for the pipes to close, then stops
--   reading and reports 'CaptureStoppedEarly'.
--
-- * __No child is leaked.__ If the calling thread receives an exception
--   (for example an interrupt), the process group is stopped the same way
--   before the exception continues.
--
-- == Platform notes
--
-- Process groups are a POSIX feature. On Windows only the build tool's own
-- process is terminated on a timeout; processes it started may survive.
--
-- Because the build tool runs in its own process group, a terminal
-- interrupt (Ctrl-C) reaches this program and not the build tool directly;
-- this program then stops the build tool's group as described above.
module Tadka.GHCProtocol.Runner
  ( runBuild
  , runCaptured
  , RunSettings (..)
  , defaultRunSettings
  , BuildRunner (..)
  , ioBuildRunner
  ) where

import Control.Concurrent.Async (Async, AsyncCancelled (..), cancel, waitCatch, withAsync)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (IOException, SomeException, finally, fromException, onException, try)
import Control.Monad (void, when)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose)
import qualified System.Process
import qualified System.Timeout
import System.Process.Typed
  ( Process
  , closed
  , createPipe
  , getStderr
  , getStdout
  , proc
  , setCreateGroup
  , setStderr
  , setStdin
  , setStdout
  , setWorkingDir
  , startProcess
  , stopProcess
  , unsafeProcessHandle
  , waitExitCode
  )
#if !defined(mingw32_HOST_OS)
import qualified System.Posix.Signals as Posix
#endif

import Tadka.GHCProtocol.BuildTool
  (BuildTool (..), FramerState, emptyFramerState, feedChunk, flushFramer)
import Tadka.GHCProtocol.Process
  ( BuildResult (..)
  , CaptureStatus (..)
  , CompilerResult (..)
  , OutputStream (..)
  , ProcessError (..)
  , Signal (..)
  , Timeout
  , timeoutMicroseconds
  )

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

--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------

-- | How long the collector is willing to wait at the two points where a
-- process the build started could otherwise make it wait indefinitely.
-- Both are in microseconds.
data RunSettings = RunSettings
  { terminateGraceMicros :: Int
    -- ^ After a timeout has asked the build's process group to stop
    -- (SIGTERM): how long to wait for the build tool to exit and its output
    -- to end before killing the group (SIGKILL).
  , drainGraceMicros     :: Int
    -- ^ After the build tool has exited (or been killed): how long to keep
    -- reading while a process it started still holds an output pipe open,
    -- before giving up and reporting 'CaptureStoppedEarly'.
  } deriving stock (Eq, Show)

-- | Three seconds to stop gracefully, two seconds to finish reading.
defaultRunSettings :: RunSettings
defaultRunSettings = RunSettings
  { terminateGraceMicros = 3000000
  , drainGraceMicros     = 2000000
  }

--------------------------------------------------------------------------------
-- Collecting output
--------------------------------------------------------------------------------

-- | Appends one completed line to the single shared, thread-safe output
-- buffer both reader threads push into, preserving the order each
-- thread observed its own chunks arriving in -- this is what gives the
-- overall result its "observed order" guarantee, not a claim about the
-- child's true cross-stream write order (I-19).
appendLine :: TVar [(OutputStream, BS.ByteString)] -> OutputStream -> BS.ByteString -> IO ()
appendLine var stream line = atomically (modifyTVar' var ((stream, line) :))

-- | Reads chunks from one pipe, feeding each into that stream's own
-- 'FramerState' (never shared with the other stream, per §6.4's
-- independence requirement) and appending every completed line to the
-- shared accumulator as it is observed.
--
-- Returns 'Nothing' when the pipe reached its end, or @Just reason@ when
-- reading failed. In every case, including cancellation by another thread,
-- a final line with no newline is flushed into the accumulator: nothing
-- buffered is ever lost.
drainLoop
  :: OutputStream -> Handle -> IORef FramerState
  -> TVar [(OutputStream, BS.ByteString)] -> IO (Maybe Text)
drainLoop stream h framerRef accumVar = loop `finally` flushPartial
  where
    loop = do
      attempt <- try (BS.hGetSome h 4096)
      case attempt of
        Left (e :: IOException) -> pure (Just (Text.pack (show e)))
        Right chunk
          | BS.null chunk -> pure Nothing
          | otherwise -> do
              framer <- readIORef framerRef
              let (framer', completed) = feedChunk framer chunk
              writeIORef framerRef framer'
              mapM_ (appendLine accumVar stream) completed
              loop

    flushPartial = do
      framer <- readIORef framerRef
      writeIORef framerRef emptyFramerState
      mapM_ (appendLine accumVar stream) (flushFramer framer)

-- | The outcome of one reader: its own result, or the exception that ended
-- it (never expected, but never ignored).
type ReaderResult = Either SomeException (Maybe Text)

-- | Waits for both readers, at most the given time. If they finish, their
-- results decide the status. If not, both are cancelled (each keeps what it
-- had), and the status says reading stopped early -- unless a reader had
-- already failed, in which case that failure is the more specific fact.
drainWithin :: Int -> Async (Maybe Text) -> Async (Maybe Text) -> IO CaptureStatus
drainWithin micros outA errA = do
  finished <- System.Timeout.timeout micros ((,) <$> waitCatch outA <*> waitCatch errA)
  case finished of
    Just (a, b) -> pure (statusFrom [a, b])
    Nothing     -> do
      cancel outA
      cancel errA
      a <- waitCatch outA
      b <- waitCatch errA
      pure $ case statusFrom [a, b] of
        CaptureComplete -> CaptureStoppedEarly
        failure         -> failure

-- | The first genuine failure among the readers' results, if any. A reader
-- that was cancelled by 'drainWithin' is not a failure: it is the very
-- thing 'CaptureStoppedEarly' reports.
statusFrom :: [ReaderResult] -> CaptureStatus
statusFrom results =
  case [msg | Right (Just msg) <- results] ++ [Text.pack (show e) | Left e <- results, not (cancelled e)] of
    []      -> CaptureComplete
    (m : _) -> CaptureReadFailed m
  where
    cancelled e = case fromException e of
      Just AsyncCancelled -> True
      Nothing             -> False

--------------------------------------------------------------------------------
-- Stopping a build: its whole process group on POSIX
--------------------------------------------------------------------------------

data GroupSignal = GroupTerminate | GroupKill

-- | Identifies the build tool's process group. On POSIX the child is
-- started as the leader of a new group, so the group id is its process id.
-- Captured right after the start, before the child can exit.
captureGroup :: Process i o e -> IO (Maybe System.Process.Pid)
captureGroup p = System.Process.getPid (unsafeProcessHandle p)

-- | Sends a signal to the build's process group. Never throws: a group that
-- is already gone is not an error. Where there are no process groups
-- (Windows), or the group id is unknown, only the build tool's own process
-- is terminated.
signalGroup :: Process i o e -> Maybe System.Process.Pid -> GroupSignal -> IO ()
signalGroup p mPid signal = ignoreIOException $
#if defined(mingw32_HOST_OS)
  System.Process.terminateProcess (unsafeProcessHandle p)
  where _ = (mPid, signal)
#else
  case mPid of
    Just pgid -> Posix.signalProcessGroup (toPosix signal) pgid
    Nothing   -> System.Process.terminateProcess (unsafeProcessHandle p)
  where
    toPosix GroupTerminate = Posix.sigTERM
    toPosix GroupKill      = Posix.sigKILL
#endif

ignoreIOException :: IO () -> IO ()
ignoreIOException act = void (try act :: IO (Either IOException ()))

-- | Asks the group to stop, waits up to the grace period for the build tool
-- to exit, then kills the group and reaps the build tool. Used when the
-- calling thread is being interrupted.
terminateGroup :: RunSettings -> Process i o e -> Maybe System.Process.Pid -> IO ()
terminateGroup settings p mPid = do
  signalGroup p mPid GroupTerminate
  exited <- System.Timeout.timeout (terminateGraceMicros settings) (waitExitCode p)
  when (isNothing exited) $ do
    signalGroup p mPid GroupKill
    void (waitExitCode p)

--------------------------------------------------------------------------------
-- The lifecycle
--------------------------------------------------------------------------------

-- | Runs a build tool and captures everything it prints. See the module
-- header for the guarantees. A start failure (executable not found,
-- permission denied, missing working directory) is caught and reported as
-- 'CompilerStartFailed'; nothing past that point runs and no
-- partially-spawned process is left behind.
runBuild :: Maybe Timeout -> BuildTool -> FilePath -> [Text] -> IO BuildResult
runBuild mTimeout tool workDir extraArgs =
  runCaptured defaultRunSettings mTimeout workDir
    (buildToolExecutable tool) ("build" : map Text.unpack extraArgs)

-- | The general form of 'runBuild': run any executable in a working
-- directory with explicit arguments and capture its output, with the same
-- guarantees and with the grace periods under the caller's control.
-- 'runBuild' is this with the build tool's name and @build@ as the first
-- argument.
runCaptured
  :: RunSettings -> Maybe Timeout -> FilePath -> FilePath -> [String] -> IO BuildResult
runCaptured settings mTimeout workDir exe args = do
  accumVar     <- newTVarIO []
  outFramerRef <- newIORef emptyFramerState
  errFramerRef <- newIORef emptyFramerState
  let pc = setCreateGroup True
         $ setStdin closed
         $ setStdout createPipe
         $ setStderr createPipe
         $ setWorkingDir workDir
         $ proc exe args

  spawnAttempt <- try (startProcess pc) :: IO (Either IOException (Process () Handle Handle))
  case spawnAttempt of
    Left ioErr ->
      pure (BuildResult (CompilerStartFailed (ProcessError (Text.pack (show ioErr)))) [] CaptureComplete)
    Right p -> do
      mPid <- captureGroup p
      (compResult, status) <-
        supervise settings mTimeout p mPid outFramerRef errFramerRef accumVar
          `onException` terminateGroup settings p mPid
          `finally` (closePipe (getStdout p) >> closePipe (getStderr p) >> stopProcess p)
      observed <- reverse <$> readTVarIO accumVar
      pure (BuildResult compResult observed status)
  where
    closePipe h = ignoreIOException (hClose h)

-- | Starts the two readers, waits for the build tool to exit or the timeout
-- to expire, and brings everything to a bounded end.
supervise
  :: RunSettings -> Maybe Timeout -> Process () Handle Handle -> Maybe System.Process.Pid
  -> IORef FramerState -> IORef FramerState -> TVar [(OutputStream, BS.ByteString)]
  -> IO (CompilerResult, CaptureStatus)
supervise settings mTimeout p mPid outFramerRef errFramerRef accumVar =
  withAsync (drainLoop StdOut (getStdout p) outFramerRef accumVar) $ \outA ->
  withAsync (drainLoop StdErr (getStderr p) errFramerRef accumVar) $ \errA -> do
    mExit <- case mTimeout of
      Nothing -> Just <$> waitExitCode p
      Just t  -> System.Timeout.timeout (timeoutMicroseconds t) (waitExitCode p)

    case mExit of
      Just ec -> do
        -- The build tool exited by itself. Its pipes normally close with
        -- it; a process it started may hold them longer, so this wait is
        -- bounded.
        status <- drainWithin (drainGraceMicros settings) outA errA
        pure (resultFromExitCode ec, status)

      Nothing -> do
        -- Timed out. Ask the whole group to stop and give it a short time
        -- to exit and close its pipes; kill whatever is left.
        signalGroup p mPid GroupTerminate
        gracefully <- System.Timeout.timeout (terminateGraceMicros settings)
                        (waitExitCode p >> waitCatch outA >> waitCatch errA)
        when (isNothing gracefully) $ do
          signalGroup p mPid GroupKill
          void (waitExitCode p)
        status <- drainWithin (drainGraceMicros settings) outA errA
        pure (CompilerTimedOut, status)

--------------------------------------------------------------------------------
-- Deterministic testing capability (spec Phase 9.4): a fake 'BuildRunner'
-- lets the full test suite run without a real cabal/stack/GHC toolchain.
--------------------------------------------------------------------------------

-- | A way to run a build, abstracted so tests can substitute a fake.
newtype BuildRunner m = BuildRunner
  { execute :: Maybe Timeout -> BuildTool -> FilePath -> [Text] -> m BuildResult }

-- | The real thing: 'runBuild'.
ioBuildRunner :: BuildRunner IO
ioBuildRunner = BuildRunner runBuild
