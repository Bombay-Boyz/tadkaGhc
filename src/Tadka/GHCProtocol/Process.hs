{-# OPTIONS_HADDOCK hide #-}
-- | Process-outcome types (vision §3; spec Phase 6.1), kept pure and
-- re-exported from the core library so 'Tadka.GHCProtocol.Opaque'
-- (Phase 7) can refer to them without depending on the
-- process-spawning 'tadka-ghc-process' component that actually
-- produces them (I-35).
module Tadka.GHCProtocol.Process
  ( OutputStream (..)
  , CompilerResult (..)
  , Signal (..)
  , ProcessError (..)
  , renderCompilerResult
  , CaptureStatus (..)
  , renderCaptureStatus
  , BuildResult (..)
    -- * Build timeout
  , Timeout
  , TimeoutError (..)
  , mkTimeout
  , timeoutMicroseconds
  , maxTimeoutSeconds
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))

-- | Which of the build tool's two output streams a captured line arrived
-- on.
data OutputStream = StdOut | StdErr
  deriving stock (Eq, Show)

-- | A timeout and a signalled termination are both genuinely distinct
-- from an ordinary non-zero exit, and from each other; a process that
-- never started at all is distinct again from one that started and
-- then failed.
data CompilerResult
  = CompilerSucceeded
  | CompilerFailed ExitCode
  | CompilerSignalled Signal
  | CompilerTimedOut
  | CompilerStartFailed ProcessError
  deriving stock (Eq, Show)

-- | The POSIX/OS signal number that terminated the child process, where
-- the runtime is able to surface one.
newtype Signal = Signal Int
  deriving stock (Eq, Show)

-- | Wraps the underlying IO failure when the child process itself could
-- not be started (executable not found, permission denied, working
-- directory missing, etc.).
newtype ProcessError = ProcessError Text
  deriving stock (Eq, Show)

-- | Why a build did not succeed, as one plain sentence; 'Nothing' for
-- 'CompilerSucceeded'. This is what a user is told when the build
-- produced no output of its own to show (a tool that could not be
-- started, a timeout, a signal, or a silent non-zero exit).
renderCompilerResult :: CompilerResult -> Maybe Text
renderCompilerResult result = case result of
  CompilerSucceeded                      -> Nothing
  CompilerFailed ExitSuccess             -> Just "the build was reported as failed although its exit status was 0"
  CompilerFailed (ExitFailure n)         -> Just ("the build failed with exit status " <> showInt n)
  CompilerSignalled (Signal n)           -> Just ("the build was killed by signal " <> showInt n)
  CompilerTimedOut                       -> Just "the build did not finish within the timeout and was stopped"
  CompilerStartFailed (ProcessError msg) -> Just ("the build tool could not be started: " <> msg)
  where
    showInt :: Int -> Text
    showInt = Text.pack . show

-- | Whether everything the build printed was captured. The build tool's
-- output pipes can be held open by a process the build started (a compiler,
-- a daemon) long after the build tool itself exited, so the collector
-- waits only a bounded time for the end of the output; when it gives up, or
-- when reading fails, that is recorded here rather than silently losing
-- lines.
data CaptureStatus
  = CaptureComplete
    -- ^ Both output streams were read to their end.
  | CaptureStoppedEarly
    -- ^ Reading stopped before the end of the output because a process
    -- started by the build was still holding an output pipe open. Output
    -- written after that point is missing; what was already received,
    -- including a final partial line, is kept.
  | CaptureReadFailed Text
    -- ^ Reading an output pipe failed with this error. What was already
    -- received, including a final partial line, is kept.
  deriving stock (Eq, Show)

-- | One plain sentence saying why the captured output may be incomplete;
-- 'Nothing' when the capture is 'CaptureComplete'.
renderCaptureStatus :: CaptureStatus -> Maybe Text
renderCaptureStatus status = case status of
  CaptureComplete       -> Nothing
  CaptureStoppedEarly   ->
    Just "stopped reading the build's output before it ended, because a process started by the build still held it open; later output may be missing"
  CaptureReadFailed msg ->
    Just ("reading the build's output failed (" <> msg <> "); output after that point may be missing")

-- | Everything observed about one finished build: how the process ended,
-- and every output line it produced.
data BuildResult = BuildResult
  { compilerResult :: CompilerResult
    -- ^ A process-level fact, never inferred from whether captured
    -- lines happened to decode successfully.
  , observedOutput :: [(OutputStream, ByteString)]
    -- ^ In the order this collector observed the two pipes deliver
    -- data -- not a guarantee of the child's true cross-stream write
    -- order (I-19): stdout and stderr are independent OS pipes.
  , captureStatus  :: CaptureStatus
    -- ^ Whether 'observedOutput' is the whole output or a bounded part
    -- of it.
  } deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Build timeout (audit B13).
--
-- 'System.Timeout.timeout' treats a negative duration as "no timeout" and
-- zero as "fire immediately", and an out-of-range 'Int' wraps. A raw
-- number must therefore never reach it. 'Timeout' makes the invalid
-- values unrepresentable: the constructor is not exported, so every
-- 'Timeout' in existence came through 'mkTimeout'.
--------------------------------------------------------------------------------

-- | A validated build timeout, stored as whole microseconds (the unit
-- 'System.Timeout.timeout' takes, so the runner never converts or
-- rounds). Always at least 1 and never more than 'maxTimeoutSeconds'.
newtype Timeout = Timeout Int
  deriving stock (Eq, Ord, Show)

-- | Why 'mkTimeout' rejected a value.
data TimeoutError
  = TimeoutNotFinite    -- ^ NaN or infinite
  | TimeoutNotPositive  -- ^ zero or negative
  | TimeoutTooLarge     -- ^ beyond 'maxTimeoutSeconds', or not representable as microseconds in an 'Int'
  deriving stock (Eq, Show)

-- | The longest timeout accepted: one year. A policy limit, far below
-- anything an 'Int' of microseconds can hold on a 64-bit platform.
maxTimeoutSeconds :: Double
maxTimeoutSeconds = 31536000

-- | Total. Accepts exactly the finite values in @(0, 'maxTimeoutSeconds']@
-- that fit in an 'Int' number of microseconds on this platform. The
-- value is rounded /up/ to a whole microsecond, so a tiny positive
-- number becomes 1 microsecond rather than 0 (which would fire
-- immediately). The range check is done in 'Integer', so it cannot
-- overflow on a 32-bit 'Int'.
mkTimeout :: Double -> Either TimeoutError Timeout
mkTimeout seconds
  | isNaN seconds || isInfinite seconds   = Left TimeoutNotFinite
  | seconds <= 0                          = Left TimeoutNotPositive
  | seconds > maxTimeoutSeconds           = Left TimeoutTooLarge
  | micros > toInteger (maxBound :: Int)  = Left TimeoutTooLarge
  | otherwise                             = Right (Timeout (fromInteger micros))
  where
    micros :: Integer
    micros = ceiling (seconds * 1000000)

-- | The timeout in microseconds, ready for 'System.Timeout.timeout'.
-- Always positive.
timeoutMicroseconds :: Timeout -> Int
timeoutMicroseconds (Timeout us) = us
