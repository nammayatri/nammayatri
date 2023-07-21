{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Merchant.MerchantServiceUsageConfig where

import Data.Singletons.TH
import qualified Domain.Types.Merchant as Domain
import qualified Domain.Types.Merchant.MerchantServiceUsageConfig as Domain
import Kernel.External.Call.Types (CallService)
import Kernel.External.Maps.Types
import Kernel.External.Notification.Types (NotificationService)
import Kernel.External.SMS (SmsService)
import Kernel.External.Whatsapp.Types (WhatsappService)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common (Log, decodeFromText, encodeToText)
import Kernel.Utils.Error
import Storage.Tabular.Merchant (MerchantTId)
import Tools.Error

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantServiceUsageConfigT sql=merchant_service_usage_config
      merchantId MerchantTId
      initiateCall CallService
      getDistances (SMapsService 'GetDistances)
      getDistancesPercentage Text
      getRoutes (SMapsService 'GetRoutes)
      getRoutesPercentage Text
      snapToRoad (SMapsService 'SnapToRoad)
      snapToRoadPercentage Text
      getPlaceName (SMapsService 'GetPlaceName)
      getPlaceNamePercentage Text
      getPickupRoutes (SMapsService 'GetPickupRoutes)
      getPickupRoutesPercentage Text
      getTripRoutes (SMapsService 'GetTripRoutes)
      getTripRoutesPercentage Text
      getPlaceDetails (SMapsService 'GetPlaceDetails)
      getPlaceDetailsPercentage Text
      autoComplete (SMapsService 'AutoComplete)
      autoCompletePercentage Text
      getDistancesForCancelRide (SMapsService 'GetDistancesForCancelRide)
      getDistancesForCancelRidePercentage Text
      notifyPerson NotificationService
      useFraudDetection Bool
      smsProvidersPriorityList (PostgresList SmsService)
      whatsappProvidersPriorityList (PostgresList WhatsappService)
      updatedAt UTCTime
      createdAt UTCTime
      Primary merchantId
      deriving Generic
    |]

instance TEntityKey MerchantServiceUsageConfigT where
  type DomainKey MerchantServiceUsageConfigT = Id Domain.Merchant
  fromKey (MerchantServiceUsageConfigTKey _id) = fromKey _id
  toKey id = MerchantServiceUsageConfigTKey $ toKey id

instance FromTType MerchantServiceUsageConfigT Domain.MerchantServiceUsageConfig where
  fromTType MerchantServiceUsageConfigT {..} = do
    getDistances' <- parseField getDistances getDistancesPercentage
    getRoutes' <- parseField getRoutes getRoutesPercentage
    getPickupRoutes' <- parseField getPickupRoutes getPickupRoutesPercentage
    getTripRoutes' <- parseField getTripRoutes getTripRoutesPercentage
    snapToRoad' <- parseField snapToRoad snapToRoadPercentage
    getPlaceName' <- parseField getPlaceName getPlaceNamePercentage
    getPlaceDetails' <- parseField getPlaceDetails getPlaceDetailsPercentage
    autoComplete' <- parseField autoComplete autoCompletePercentage
    getDistancesForCancelRide' <- parseField getDistancesForCancelRide getDistancesForCancelRidePercentage
    return $
      Domain.MerchantServiceUsageConfig
        { merchantId = fromKey merchantId,
          smsProvidersPriorityList = unPostgresList smsProvidersPriorityList,
          whatsappProvidersPriorityList = unPostgresList whatsappProvidersPriorityList,
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
        forall (msum :: MapsServiceUsageMethod) m.
        (SingI msum, MonadThrow m, Log m) =>
        SMapsService msum ->
        Text ->
        m (MapsServiceUsage msum)
      parseField field fieldPercentage = do
        let mapsServiceUsageMethod = fromSing (sing @msum)
        let fieldName = show mapsServiceUsageMethod
        mapsServiceUsagePercentage <-
          decodeFromText fieldPercentage
            & fromMaybeM (InternalError $ "Unable to decode MerchantServiceUsageConfigT." <> fieldName <> "Percentage")
        pure $ mkMapsServiceUsage field mapsServiceUsagePercentage

instance ToTType MerchantServiceUsageConfigT Domain.MerchantServiceUsageConfig where
  toTType Domain.MerchantServiceUsageConfig {..} = do
    let mkPercentage = encodeToText . mkMapsServiceUsagePercentage
    MerchantServiceUsageConfigT
      { merchantId = toKey merchantId,
        smsProvidersPriorityList = PostgresList smsProvidersPriorityList,
        whatsappProvidersPriorityList = PostgresList whatsappProvidersPriorityList,
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
