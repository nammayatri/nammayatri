{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.VehicleServiceTier where

import qualified Domain.Types.VehicleVariant as VehicleVariant
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Utils.GenericPretty
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data VehicleServiceTierType = SEDAN | SUV | HATCHBACK | AUTO_RICKSHAW | TAXI | TAXI_PLUS | ECO | COMFY | PREMIUM
  deriving
    ( Show,
      Eq,
      Read,
      Ord,
      Generic,
      ToJSON,
      FromJSON,
      ToSchema,
      ToParamSchema,
      Enum,
      Bounded
    )
  deriving (PrettyShow) via Showable VehicleServiceTierType

derivePersistField "VehicleServiceTierType"

$(mkBeamInstancesForEnum ''VehicleServiceTierType)

castServiceTierToVariant :: VehicleServiceTierType -> VehicleVariant.VehicleVariant
castServiceTierToVariant SEDAN = VehicleVariant.SEDAN
castServiceTierToVariant SUV = VehicleVariant.SUV
castServiceTierToVariant HATCHBACK = VehicleVariant.HATCHBACK
castServiceTierToVariant AUTO_RICKSHAW = VehicleVariant.AUTO_RICKSHAW
castServiceTierToVariant TAXI = VehicleVariant.TAXI
castServiceTierToVariant TAXI_PLUS = VehicleVariant.TAXI_PLUS
castServiceTierToVariant ECO = VehicleVariant.HATCHBACK
castServiceTierToVariant COMFY = VehicleVariant.SEDAN
castServiceTierToVariant PREMIUM = VehicleVariant.SEDAN

castVariantToServiceTier :: VehicleVariant.VehicleVariant -> VehicleServiceTierType
castVariantToServiceTier VehicleVariant.SEDAN = SEDAN
castVariantToServiceTier VehicleVariant.SUV = SUV
castVariantToServiceTier VehicleVariant.HATCHBACK = HATCHBACK
castVariantToServiceTier VehicleVariant.AUTO_RICKSHAW = AUTO_RICKSHAW
castVariantToServiceTier VehicleVariant.TAXI = TAXI
castVariantToServiceTier VehicleVariant.TAXI_PLUS = TAXI_PLUS
