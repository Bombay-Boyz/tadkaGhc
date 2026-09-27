-- | The supported public entry point (spec §29/Phase 2's module layout).
-- Only this module -- not 'Tadka.GHCProtocol.Types'/'.Schema'/'.Decode'
-- directly -- is a compatibility commitment; those stay reachable for
-- internal package use but their contents may be reorganized freely.
module Tadka.GHCProtocol
  ( module Tadka.GHCProtocol.Types
  , module Tadka.GHCProtocol.Decode
  ) where

import Tadka.GHCProtocol.Decode
import Tadka.GHCProtocol.Types
