-- | The supported public entry point (spec §29/Phase 2's module layout).
-- Deliberately does NOT re-export 'Tadka.GHCProtocol.Runner': that lives
-- in the separate 'tadka-ghc-process' component (I-35).
module Tadka.GHCProtocol
  ( module Tadka.GHCProtocol.Types
  , module Tadka.GHCProtocol.Decode
  , module Tadka.GHCProtocol.Span
  , module Tadka.GHCProtocol.Diagnostic
  , module Tadka.GHCProtocol.BuildTool
  , module Tadka.GHCProtocol.Process
  , module Tadka.GHCProtocol.Opaque
  ) where

import Tadka.GHCProtocol.BuildTool
import Tadka.GHCProtocol.Decode
import Tadka.GHCProtocol.Diagnostic
import Tadka.GHCProtocol.Opaque
import Tadka.GHCProtocol.Process
import Tadka.GHCProtocol.Span
import Tadka.GHCProtocol.Types
