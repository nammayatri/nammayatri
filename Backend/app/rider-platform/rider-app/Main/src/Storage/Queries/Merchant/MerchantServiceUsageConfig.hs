{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Merchant.MerchantServiceUsageConfig
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Database.Beam.Postgres (Postgres)
import Domain.Types.Merchant.MerchantServiceUsageConfig
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import Kernel.Beam.Functions
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.MerchantServiceUsageConfig as BeamMSUC

findByMerchantOperatingCityId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id MerchantOperatingCity -> m (Maybe MerchantServiceUsageConfig)
findByMerchantOperatingCityId (Id merchantOperatingCityId) = findOneWithKV [Se.Is BeamMSUC.merchantOperatingCityId $ Se.Eq merchantOperatingCityId]

updateMerchantServiceUsageConfig :: MonadFlow m => MerchantServiceUsageConfig -> m ()
updateMerchantServiceUsageConfig MerchantServiceUsageConfig {..} = do
  now <- getCurrentTime
  updateWithKV
    ( concat
        [ updUsage BeamMSUC.getDistances BeamMSUC.getDistancesPercentage getDistances,
          updUsage BeamMSUC.getRoutes BeamMSUC.getRoutesPercentage getRoutes,
          updUsage BeamMSUC.getPickupRoutes BeamMSUC.getPickupRoutesPercentage getPickupRoutes,
          updUsage BeamMSUC.getTripRoutes BeamMSUC.getTripRoutesPercentage getTripRoutes,
          updUsage BeamMSUC.snapToRoad BeamMSUC.snapToRoadPercentage snapToRoad,
          updUsage BeamMSUC.getPlaceName BeamMSUC.getPlaceNamePercentage getPlaceName,
          updUsage BeamMSUC.getPlaceDetails BeamMSUC.getPlaceDetailsPercentage getPlaceDetails,
          updUsage BeamMSUC.autoComplete BeamMSUC.autoCompletePercentage autoComplete,
          updUsage BeamMSUC.getDistancesForCancelRide BeamMSUC.getDistancesForCancelRidePercentage getDistancesForCancelRide,
          [ Se.Set BeamMSUC.smsProvidersPriorityList smsProvidersPriorityList,
            Se.Set BeamMSUC.updatedAt now
          ]
        ]
    )
    [Se.Is BeamMSUC.merchantOperatingCityId (Se.Eq $ getId merchantOperatingCityId)]
  where
    updUsage ::
      Se.Column BeamMSUC.MerchantServiceUsageConfigT Maps.MapsService ->
      Se.Column BeamMSUC.MerchantServiceUsageConfigT Text ->
      Maps.MapsServiceUsage ->
      [Se.Set Postgres BeamMSUC.MerchantServiceUsageConfigT]
    updUsage dbField dbFieldPercentage dField =
      [ Se.Set dbField dField.mapsService,
        Se.Set dbFieldPercentage (encodeToText . Maps.mkMapsServiceUsagePercentage $ dField)
      ]

instance FromTType' BeamMSUC.MerchantServiceUsageConfig MerchantServiceUsageConfig where
  fromTType' BeamMSUC.MerchantServiceUsageConfigT {..} = do
    getDistances' <- parseField Maps.GetDistances getDistances getDistancesPercentage
    getRoutes' <- parseField Maps.GetRoutes getRoutes getRoutesPercentage
    getPickupRoutes' <- parseField Maps.GetPickupRoutes getPickupRoutes getPickupRoutesPercentage
    getTripRoutes' <- parseField Maps.GetTripRoutes getTripRoutes getTripRoutesPercentage
    snapToRoad' <- parseField Maps.SnapToRoad snapToRoad snapToRoadPercentage
    getPlaceName' <- parseField Maps.GetPlaceName getPlaceName getPlaceNamePercentage
    getPlaceDetails' <- parseField Maps.GetPlaceDetails getPlaceDetails getPlaceDetailsPercentage
    autoComplete' <- parseField Maps.AutoComplete autoComplete autoCompletePercentage
    getDistancesForCancelRide' <- parseField Maps.GetDistancesForCancelRide getDistancesForCancelRide getDistancesForCancelRidePercentage
    pure $
      Just
        MerchantServiceUsageConfig
          { merchantId = Id merchantId,
            merchantOperatingCityId = Id merchantOperatingCityId,
            initiateCall = initiateCall,
            notifyPerson = notifyPerson,
            aadhaarVerificationService = aadhaarVerificationService,
            smsProvidersPriorityList = smsProvidersPriorityList,
            whatsappProvidersPriorityList = whatsappProvidersPriorityList,
            issueTicketService = issueTicketService,
            useFraudDetection = useFraudDetection,
            enableDashboardSms = enableDashboardSms,
            getExophone = getExophone,
            updatedAt = updatedAt,
            createdAt = createdAt,
            getDistances = getDistances',
            getRoutes = getRoutes',
            getPickupRoutes = getPickupRoutes',
            getTripRoutes = getTripRoutes',
            snapToRoad = snapToRoad',
            getPlaceName = getPlaceName',
            getPlaceDetails = getPlaceDetails',
            autoComplete = autoComplete',
            getDistancesForCancelRide = getDistancesForCancelRide',
            ..
          }
    where
      parseField ::
        (MonadThrow m, Log m) =>
        Maps.MapsServiceUsageMethod ->
        Maps.MapsService ->
        Text ->
        m Maps.MapsServiceUsage
      parseField mapsServiceUsageMethod field fieldPercentage = do
        let fieldName = show mapsServiceUsageMethod
        mapsServiceUsagePercentage <-
          decodeFromText fieldPercentage
            & fromMaybeM (InternalError $ "Unable to decode MerchantServiceUsageConfigT." <> fieldName <> "Percentage")
        pure $ Maps.mkMapsServiceUsage field mapsServiceUsagePercentage

instance ToTType' BeamMSUC.MerchantServiceUsageConfig MerchantServiceUsageConfig where
  toTType' MerchantServiceUsageConfig {..} = do
    let mkPercentage = encodeToText . Maps.mkMapsServiceUsagePercentage
    BeamMSUC.MerchantServiceUsageConfigT
      { BeamMSUC.merchantId = getId merchantId,
        BeamMSUC.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamMSUC.getDistances = getDistances.mapsService,
        BeamMSUC.getDistancesPercentage = mkPercentage getDistances,
        BeamMSUC.getRoutes = getRoutes.mapsService,
        BeamMSUC.getRoutesPercentage = mkPercentage getRoutes,
        BeamMSUC.snapToRoad = snapToRoad.mapsService,
        BeamMSUC.snapToRoadPercentage = mkPercentage snapToRoad,
        BeamMSUC.getPlaceName = getPlaceName.mapsService,
        BeamMSUC.getPlaceNamePercentage = mkPercentage getPlaceName,
        BeamMSUC.getPickupRoutes = getPickupRoutes.mapsService,
        BeamMSUC.getPickupRoutesPercentage = mkPercentage getPickupRoutes,
        BeamMSUC.getTripRoutes = getTripRoutes.mapsService,
        BeamMSUC.getTripRoutesPercentage = mkPercentage getTripRoutes,
        BeamMSUC.getPlaceDetails = getPlaceDetails.mapsService,
        BeamMSUC.getPlaceDetailsPercentage = mkPercentage getPlaceDetails,
        BeamMSUC.autoComplete = autoComplete.mapsService,
        BeamMSUC.autoCompletePercentage = mkPercentage autoComplete,
        BeamMSUC.getDistancesForCancelRide = getDistancesForCancelRide.mapsService,
        BeamMSUC.getDistancesForCancelRidePercentage = mkPercentage getDistancesForCancelRide,
        ..
      }
