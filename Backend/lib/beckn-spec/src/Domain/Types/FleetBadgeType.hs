module Domain.Types.FleetBadgeType where

import Kernel.Prelude

data FleetBadgeType = DRIVER | CONDUCTOR deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)
