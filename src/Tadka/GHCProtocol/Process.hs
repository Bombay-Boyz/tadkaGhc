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
  deriving stock (Show)

-- | The POSIX/OS signal number that terminated the child process, where
-- the runtime is able to surface one.
newtype Signal = Signal Int
  deriving stock (Eq, Show)

-- | Wraps the underlying IO failure when the child process itself could
-- not be started (executable not found, permission denied, working
-- directory missing, etc.).
newtype ProcessError = ProcessError Text
  deriving stock (Show)

data BuildResult = BuildResult
  { compilerResult :: CompilerResult
    -- ^ A process-level fact, never inferred from whether captured
    -- lines happened to decode successfully.
  , observedOutput :: [(OutputStream, ByteString)]
    -- ^ In the order this collector observed the two pipes deliver
    -- data -- not a guarantee of the child's true cross-stream write
    -- order (I-19): stdout and stderr are independent OS pipes.
  } deriving stock (Show)
