{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Internal.Estimate where

import Domain.Types.Common
import Domain.Types.Estimate
import Domain.Types.FareParameters
import Domain.Types.FarePolicy
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import Domain.Types.SearchRequest
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Merchant as QM
import Tools.Error

data BppEstimate = BppEstimate
  { congestionMultiplier :: Maybe Centesimal,
    createdAt :: UTCTime,
    currency :: Currency,
    distanceUnit :: DistanceUnit,
    dpVersion :: Maybe Text,
    eligibleForUpgrade :: Bool,
    estimatedDistance :: Maybe Meters,
    estimatedDuration :: Maybe Seconds,
    fareParamsId :: Maybe (Id FareParameters),
    farePolicyId :: Maybe (Id FarePolicy),
    fromLocGeohash :: Maybe Text,
    id :: Kernel.Types.Id.Id Estimate,
    isBlockedRoute :: Maybe Bool,
    isCustomerPrefferedSearchRoute :: Maybe Bool,
    isScheduled :: Bool,
    maxFare :: HighPrecMoney,
    mbActualQARCity :: Maybe Double,
    mbActualQARCityPast :: Maybe Double,
    mbActualQARFromLocGeohash :: Maybe Double,
    mbActualQARFromLocGeohashDistance :: Maybe Double,
    mbActualQARFromLocGeohashDistancePast :: Maybe Double,
    mbActualQARFromLocGeohashPast :: Maybe Double,
    mbCongestionCity :: Maybe Double,
    mbCongestionCityPast :: Maybe Double,
    mbCongestionFromLocGeohash :: Maybe Double,
    mbCongestionFromLocGeohashDistance :: Maybe Double,
    mbCongestionFromLocGeohashDistancePast :: Maybe Double,
    mbCongestionFromLocGeohashPast :: Maybe Double,
    merchantId :: Maybe (Id Merchant),
    merchantOperatingCityId :: Maybe (Id MerchantOperatingCity),
    minFare :: HighPrecMoney,
    requestId :: Id SearchRequest,
    smartTipReason :: Maybe Text,
    smartTipSuggestion :: Maybe HighPrecMoney,
    specialLocationTag :: Maybe Text,
    specialLocationName :: Maybe Text,
    supplyDemandRatioFromLoc :: Maybe Double,
    supplyDemandRatioToLoc :: Maybe Double,
    tipOptions :: Maybe [Int],
    tollNames :: Maybe [Text],
    tollIds :: Maybe [Text],
    tripCategory :: TripCategory,
    updatedAt :: UTCTime,
    vehicleServiceTier :: ServiceTierType,
    vehicleServiceTierName :: Maybe Text,
    driverExtraFeeBounds :: Maybe DriverExtraFeeBounds
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

getEstimateDetails :: Id Estimate -> Maybe Text -> FlowHandler BppEstimate
getEstimateDetails estimateId apiKey = withFlowHandlerAPI $ do
  Estimate {..} <-
    QEstimate.findById estimateId
      >>= fromMaybeM (EstimateNotFound estimateId.getId)
  merchant <- case merchantId of
    Just mid -> QM.findById mid >>= fromMaybeM (MerchantDoesNotExist mid.getId)
    Nothing -> throwError $ MerchantDoesNotExist "MISSING_MERCHANT_ID"
  unless (Just merchant.internalApiKey == apiKey) $
    throwError $ AuthBlocked "Invalid BPP internal api key"
  pure $
    BppEstimate
      { fareParamsId = (.id) <$> fareParams,
        farePolicyId = (.id) <$> farePolicy,
        ..
      }
