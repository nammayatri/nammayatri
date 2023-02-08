module Beckn.Types.Core.Taxi.Common.VehicleVariant where

import Kernel.Prelude

data VehicleVariant = SEDAN | SUV | HATCHBACK | AUTO_RICKSHAW
  deriving
    ( Show,
      Eq,
      Read,
      Generic,
      ToJSON,
      FromJSON,
      ToSchema,
      ToParamSchema,
      Enum,
      Bounded
    )
