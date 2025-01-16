{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.Quote where

import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import qualified Domain.Types.FareParameters as Params
import qualified Domain.Types.FarePolicy as Policy
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.SearchRequest
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

data Quote = Quote
  { id :: Id Quote,
    searchRequestId :: Id SearchRequest,
    providerId :: Id DMerchant.Merchant,
    vehicleServiceTier :: DVST.ServiceTierType,
    vehicleServiceTierName :: Maybe Text,
    estimatedFinishTime :: Maybe UTCTime,
    tripCategory :: DTC.TripCategory,
    validTill :: UTCTime,
    estimatedFare :: HighPrecMoney,
    currency :: Currency,
    distance :: Maybe Meters,
    distanceUnit :: DistanceUnit,
    specialLocationTag :: Maybe Text,
    fareParams :: Params.FareParameters,
    farePolicy :: Maybe Policy.FarePolicy,
    isScheduled :: Bool,
    isCustomerPrefferedSearchRoute :: Maybe Bool,
    isBlockedRoute :: Maybe Bool,
    tollNames :: Maybe [Text],
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity)
  }
  deriving (Generic, Show)
