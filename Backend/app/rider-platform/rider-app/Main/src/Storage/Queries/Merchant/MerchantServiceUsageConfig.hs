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
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (findById)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Storage.Beam.Merchant.MerchantServiceUsageConfig as BeamMSUC
import Storage.Tabular.Merchant.MerchantServiceUsageConfig

findByMerchantId :: Transactionable m => Id Merchant -> m (Maybe MerchantServiceUsageConfig)
findByMerchantId merchId =
  Esq.findOne $ do
    merchantMapsCfg <- from $ table @MerchantServiceUsageConfigT
    where_ $
      merchantMapsCfg ^. MerchantServiceUsageConfigTId ==. val (toKey merchId)
    return merchantMapsCfg

updateMerchantServiceUsageConfig ::
  MerchantServiceUsageConfig ->
  SqlDB ()
updateMerchantServiceUsageConfig MerchantServiceUsageConfig {..} = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ MerchantServiceUsageConfigGetDistances =. val getDistances,
        MerchantServiceUsageConfigGetRoutes =. val getRoutes,
        MerchantServiceUsageConfigSnapToRoad =. val snapToRoad,
        MerchantServiceUsageConfigGetPlaceName =. val getPlaceName,
        MerchantServiceUsageConfigGetPlaceDetails =. val getPlaceDetails,
        MerchantServiceUsageConfigAutoComplete =. val autoComplete,
        MerchantServiceUsageConfigSmsProvidersPriorityList =. val (PostgresList smsProvidersPriorityList),
        MerchantServiceUsageConfigUpdatedAt =. val now
      ]
    where_ $
      tbl ^. MerchantServiceUsageConfigTId ==. val (toKey merchantId)

transformBeamMerchantServiceUsageConfigToDomain :: BeamMSUC.MerchantServiceUsageConfig -> MerchantServiceUsageConfig
transformBeamMerchantServiceUsageConfigToDomain BeamMSUC.MerchantServiceUsageConfigT {..} = do
  MerchantServiceUsageConfig
    { merchantId = Id merchantId,
      initiateCall = initiateCall,
      notifyPerson = notifyPerson,
      getDistances = getDistances,
      getRoutes = getRoutes,
      snapToRoad = snapToRoad,
      getPlaceName = getPlaceName,
      getPickupRoutes = getPickupRoutes,
      getTripRoutes = getTripRoutes,
      getPlaceDetails = getPlaceDetails,
      autoComplete = autoComplete,
      getDistancesForCancelRide = getDistancesForCancelRide,
      smsProvidersPriorityList = smsProvidersPriorityList,
      whatsappProvidersPriorityList = whatsappProvidersPriorityList,
      useFraudDetection = useFraudDetection,
      updatedAt = updatedAt,
      createdAt = createdAt
    }

transformDomainMerchantServiceUsageConfigToBeam :: MerchantServiceUsageConfig -> BeamMSUC.MerchantServiceUsageConfig
transformDomainMerchantServiceUsageConfigToBeam MerchantServiceUsageConfig {..} =
  BeamMSUC.defaultMerchantServiceUsageConfig
    { BeamMSUC.merchantId = getId merchantId,
      BeamMSUC.initiateCall = initiateCall,
      BeamMSUC.notifyPerson = notifyPerson,
      BeamMSUC.getDistances = getDistances,
      BeamMSUC.getRoutes = getRoutes,
      BeamMSUC.snapToRoad = snapToRoad,
      BeamMSUC.getPlaceName = getPlaceName,
      BeamMSUC.getPickupRoutes = getPickupRoutes,
      BeamMSUC.getTripRoutes = getTripRoutes,
      BeamMSUC.getPlaceDetails = getPlaceDetails,
      BeamMSUC.autoComplete = autoComplete,
      BeamMSUC.getDistancesForCancelRide = getDistancesForCancelRide,
      BeamMSUC.smsProvidersPriorityList = smsProvidersPriorityList,
      BeamMSUC.whatsappProvidersPriorityList = whatsappProvidersPriorityList,
      BeamMSUC.useFraudDetection = useFraudDetection,
      BeamMSUC.updatedAt = updatedAt,
      BeamMSUC.createdAt = createdAt
    }
