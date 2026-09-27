-- | The supported public entry point (spec §29/Phase 2's module layout).
-- Only this module -- not the individual .Types/.Schema/.Decode/.Span/
-- .Diagnostic modules directly -- is a compatibility commitment.
module Tadka.GHCProtocol
  ( module Tadka.GHCProtocol.Types
  , module Tadka.GHCProtocol.Decode
  , module Tadka.GHCProtocol.Span
  , module Tadka.GHCProtocol.Diagnostic
  ) where

import Tadka.GHCProtocol.Decode
import Tadka.GHCProtocol.Diagnostic
import Tadka.GHCProtocol.Span
import Tadka.GHCProtocol.Types
