-- | The supported public entry point (spec §29/Phase 2's module layout).
-- Only this module -- not 'Tadka.GHCProtocol.Types'/'.Schema'/'.Decode'/
-- '.Span' directly -- is a compatibility commitment; their contents may
-- be reorganized freely.
module Tadka.GHCProtocol
  ( module Tadka.GHCProtocol.Types
  , module Tadka.GHCProtocol.Decode
  , module Tadka.GHCProtocol.Span
  ) where

import Tadka.GHCProtocol.Decode
import Tadka.GHCProtocol.Span
import Tadka.GHCProtocol.Types
