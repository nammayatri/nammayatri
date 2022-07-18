module Core.ACL.Common where

import qualified Beckn.Types.Core.Taxi.Common.VehicleVariant as Common
import qualified Domain.Types.Vehicle.Variant as Variant

castVariant :: Variant.Variant -> Common.VehicleVariant
castVariant Variant.SEDAN = Common.SEDAN
castVariant Variant.HATCHBACK = Common.HATCHBACK
castVariant Variant.SUV = Common.SUV
castVariant Variant.AUTO_VARIANT = Common.AUTO
