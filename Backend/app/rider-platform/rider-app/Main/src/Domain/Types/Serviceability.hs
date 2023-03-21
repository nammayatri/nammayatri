{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Serviceability where

import Kernel.External.Maps.Types
import Kernel.Prelude
import qualified Lib.Types.SpecialLocation as DSpecialLocation

newtype ServiceabilityReq = ServiceabilityReq
  { location :: LatLong
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data ServiceabilityRes = ServiceabilityRes
  { serviceable :: Bool,
    specialLocation :: Maybe [DSpecialLocation.SpecialLocation]
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON, ToSchema)
