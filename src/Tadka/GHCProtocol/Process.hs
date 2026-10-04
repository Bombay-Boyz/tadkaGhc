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
import System.Exit (ExitCode)

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

data BuildResult = BuildResult
  { compilerResult :: CompilerResult
    -- ^ A process-level fact, never inferred from whether captured
    -- lines happened to decode successfully.
  , observedOutput :: [(OutputStream, ByteString)]
    -- ^ In the order this collector observed the two pipes deliver
    -- data -- not a guarantee of the child's true cross-stream write
    -- order (I-19): stdout and stderr are independent OS pipes.
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
