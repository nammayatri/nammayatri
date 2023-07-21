{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Merchant.MerchantServiceUsageConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant as DOrg
import Domain.Types.Merchant.MerchantServiceUsageConfig
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (findById)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (encodeToText)
import Storage.Tabular.Merchant.MerchantServiceUsageConfig

findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe MerchantServiceUsageConfig)
findByMerchantId orgId =
  Esq.findOne $ do
    orgMapsCfg <- from $ table @MerchantServiceUsageConfigT
    where_ $
      orgMapsCfg ^. MerchantServiceUsageConfigTId ==. val (toKey orgId)
    return orgMapsCfg

updateMerchantServiceUsageConfig ::
  MerchantServiceUsageConfig ->
  SqlDB ()
updateMerchantServiceUsageConfig MerchantServiceUsageConfig {..} = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    let updUsage dbField dbFieldPercentage dField =
          [ dbField =. val dField.mapsService,
            dbFieldPercentage =. (val . encodeToText . Maps.mkMapsServiceUsagePercentage $ dField)
          ]
    set
      tbl
      $ concat
        [ updUsage MerchantServiceUsageConfigGetDistances MerchantServiceUsageConfigGetDistancesPercentage getDistances,
          updUsage MerchantServiceUsageConfigGetEstimatedPickupDistances MerchantServiceUsageConfigGetEstimatedPickupDistancesPercentage getEstimatedPickupDistances,
          updUsage MerchantServiceUsageConfigGetRoutes MerchantServiceUsageConfigGetRoutesPercentage getRoutes,
          updUsage MerchantServiceUsageConfigGetPickupRoutes MerchantServiceUsageConfigGetPickupRoutesPercentage getPickupRoutes,
          updUsage MerchantServiceUsageConfigGetTripRoutes MerchantServiceUsageConfigGetTripRoutesPercentage getTripRoutes,
          updUsage MerchantServiceUsageConfigSnapToRoad MerchantServiceUsageConfigSnapToRoadPercentage snapToRoad,
          updUsage MerchantServiceUsageConfigGetPlaceName MerchantServiceUsageConfigGetPlaceNamePercentage getPlaceName,
          updUsage MerchantServiceUsageConfigGetPlaceDetails MerchantServiceUsageConfigGetPlaceDetailsPercentage getPlaceDetails,
          updUsage MerchantServiceUsageConfigAutoComplete MerchantServiceUsageConfigAutoCompletePercentage autoComplete,
          updUsage MerchantServiceUsageConfigGetDistancesForCancelRide MerchantServiceUsageConfigGetDistancesForCancelRidePercentage getDistancesForCancelRide,
          [ MerchantServiceUsageConfigSmsProvidersPriorityList =. val (PostgresList smsProvidersPriorityList),
            MerchantServiceUsageConfigUpdatedAt =. val now
          ]
        ]
    where_ $
      tbl ^. MerchantServiceUsageConfigTId ==. val (toKey merchantId)
