{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FareProductType where

import Data.Aeson
import Data.OpenApi hiding (name)
import Domain.Types.Trip
import EulerHS.Prelude hiding (length, map, readMaybe)
import Kernel.Storage.Esqueleto (derivePersistField)

-- TODO :: Deprecated, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
data FareProductType = ONE_WAY | RENTAL | DRIVER_OFFER | ONE_WAY_SPECIAL_ZONE | INTER_CITY | AMBULANCE
  deriving (Generic, Show, Read, Eq, Ord, FromJSON, ToJSON, ToSchema)

derivePersistField "FareProductType"

-- TODO :: Deprecated, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
getFareProductType :: TripCategory -> FareProductType
getFareProductType tripCategory =
  case tripCategory of
    OneWay OneWayRideOtp -> ONE_WAY_SPECIAL_ZONE
    CrossCity OneWayRideOtp _ -> ONE_WAY_SPECIAL_ZONE
    Rental _ -> RENTAL
    InterCity _ _ -> INTER_CITY
    Ambulance _ -> AMBULANCE
    _ -> DRIVER_OFFER

-- TODO :: Deprecated, please do not maintain this in future. `fareProductType` is replaced with `tripCategory`.
getTripCategory :: Maybe TripCategory -> FareProductType -> TripCategory
getTripCategory mbTripCategory fareProductType =
  case mbTripCategory of
    Just tripCategory -> tripCategory
    -- For Backward Compatibility
    Nothing ->
      case fareProductType of
        ONE_WAY -> OneWay OneWayOnDemandDynamicOffer
        DRIVER_OFFER -> OneWay OneWayOnDemandDynamicOffer
        ONE_WAY_SPECIAL_ZONE -> OneWay OneWayRideOtp
        RENTAL -> Rental OnDemandStaticOffer
        INTER_CITY -> InterCity OneWayOnDemandStaticOffer Nothing
        AMBULANCE -> Ambulance OneWayOnDemandDynamicOffer
