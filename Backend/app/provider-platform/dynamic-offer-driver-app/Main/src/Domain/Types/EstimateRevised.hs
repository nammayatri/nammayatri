{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.EstimateRevised where

import Domain.Types.Common (TripCategory (..))
import qualified Domain.Types.FareParameters as Params
import qualified Domain.Types.FarePolicy as Policy
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.Vehicle as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id

-- import qualified Domain.Types.Estimate as DEst

data EstimateRevised = EstimateRevised
  { id :: Id EstimateRevised,
    -- parentEstimateId :: Id DEst.Estimate,
    requestId :: Id DSR.SearchRequest,
    vehicleVariant :: Variant.Variant,
    minFare :: Money,
    maxFare :: Money,
    specialLocationTag :: Maybe Text,
    tripCategory :: TripCategory,
    estimatedDistance :: Maybe Meters,
    fareParams :: Maybe Params.FareParameters,
    farePolicy :: Maybe Policy.FarePolicy,
    isScheduled :: Bool,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
    -- convienceFee :: Money
  }
  deriving (Generic, Show)
