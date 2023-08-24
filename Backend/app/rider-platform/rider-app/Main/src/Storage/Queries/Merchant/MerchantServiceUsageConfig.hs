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

import Data.Singletons
import Database.Beam.Postgres (Postgres)
import Domain.Types.Merchant as DOrg
import Domain.Types.Merchant.MerchantServiceUsageConfig
import Kernel.Beam.Functions
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (decodeFromText, encodeToText, fromMaybeM)
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant.MerchantServiceUsageConfig as BeamMSUC

findByMerchantId :: MonadFlow m => Id Merchant -> m (Maybe MerchantServiceUsageConfig)
findByMerchantId (Id merchantId) = findOneWithKV [Se.Is BeamMSUC.merchantId $ Se.Eq merchantId]

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
    [Se.Is BeamMSUC.merchantId (Se.Eq $ getId merchantId)]
  where
    updUsage ::
      Se.Column BeamMSUC.MerchantServiceUsageConfigT (Maps.SMapsService msuc) ->
      Se.Column BeamMSUC.MerchantServiceUsageConfigT Text ->
      Maps.MapsServiceUsage msuc ->
      [Se.Set Postgres BeamMSUC.MerchantServiceUsageConfigT]
    updUsage dbField dbFieldPercentage dField =
      [ Se.Set dbField dField.mapsService,
        Se.Set dbFieldPercentage (encodeToText . Maps.mkMapsServiceUsagePercentage $ dField)
      ]

instance FromTType' BeamMSUC.MerchantServiceUsageConfig MerchantServiceUsageConfig where
  fromTType' BeamMSUC.MerchantServiceUsageConfigT {..} = do
    getDistances' <- parseField getDistances getDistancesPercentage
    getRoutes' <- parseField getRoutes getRoutesPercentage
    getPickupRoutes' <- parseField getPickupRoutes getPickupRoutesPercentage
    getTripRoutes' <- parseField getTripRoutes getTripRoutesPercentage
    snapToRoad' <- parseField snapToRoad snapToRoadPercentage
    getPlaceName' <- parseField getPlaceName getPlaceNamePercentage
    getPlaceDetails' <- parseField getPlaceDetails getPlaceDetailsPercentage
    autoComplete' <- parseField autoComplete autoCompletePercentage
    getDistancesForCancelRide' <- parseField getDistancesForCancelRide getDistancesForCancelRidePercentage
    pure $
      Just
        MerchantServiceUsageConfig
          { merchantId = Id merchantId,
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
        forall (msum :: Maps.MapsServiceUsageMethod) m.
        (SingI msum, MonadThrow m, Log m) =>
        Maps.SMapsService msum ->
        Text ->
        m (Maps.MapsServiceUsage msum)
      parseField field fieldPercentage = do
        let mapsServiceUsageMethod = fromSing (sing @msum)
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
        getDistances = getDistances.mapsService,
        getDistancesPercentage = mkPercentage getDistances,
        getRoutes = getRoutes.mapsService,
        getRoutesPercentage = mkPercentage getRoutes,
        getPickupRoutes = getPickupRoutes.mapsService,
        getPickupRoutesPercentage = mkPercentage getPickupRoutes,
        getTripRoutes = getTripRoutes.mapsService,
        getTripRoutesPercentage = mkPercentage getTripRoutes,
        snapToRoad = snapToRoad.mapsService,
        snapToRoadPercentage = mkPercentage snapToRoad,
        getPlaceName = getPlaceName.mapsService,
        getPlaceNamePercentage = mkPercentage getPlaceName,
        getPlaceDetails = getPlaceDetails.mapsService,
        getPlaceDetailsPercentage = mkPercentage getPlaceDetails,
        autoComplete = autoComplete.mapsService,
        autoCompletePercentage = mkPercentage autoComplete,
        getDistancesForCancelRide = getDistancesForCancelRide.mapsService,
        getDistancesForCancelRidePercentage = mkPercentage getDistancesForCancelRide,
        ..
      }
