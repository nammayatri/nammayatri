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

import qualified Domain.Types.Merchant as Domain
import qualified Domain.Types.Merchant.MerchantServiceUsageConfig as Domain
import Kernel.External.AadhaarVerification.Types
import Kernel.External.Call (CallService)
import Kernel.External.Maps.Types
import Kernel.External.SMS.Types
import Kernel.External.Verification.Types
import Kernel.External.Whatsapp.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Kernel.Utils.Common (decodeFromText, encodeToText)
import Kernel.Utils.Error
import Storage.Tabular.Merchant (MerchantTId)
import Tools.Error

mkPersist
  defaultSqlSettings
  [defaultQQ|
    MerchantServiceUsageConfigT sql=merchant_service_usage_config
      merchantId MerchantTId
      initiateCall CallService
      getDistances MapsService
      getDistancesPercentage Text
      getEstimatedPickupDistances MapsService
      getEstimatedPickupDistancesPercentage Text
      getRoutes MapsService
      getRoutesPercentage Text
      getPickupRoutes MapsService
      getPickupRoutesPercentage Text
      getTripRoutes MapsService
      getTripRoutesPercentage Text
      snapToRoad MapsService
      snapToRoadPercentage Text
      getPlaceName MapsService
      getPlaceNamePercentage Text
      getPlaceDetails MapsService
      getPlaceDetailsPercentage Text
      autoComplete MapsService
      autoCompletePercentage Text
      getDistancesForCancelRide MapsService
      getDistancesForCancelRidePercentage Text
      smsProvidersPriorityList (PostgresList SmsService)
      whatsappProvidersPriorityList (PostgresList WhatsappService)
      verificationService VerificationService
      aadhaarVerificationService AadhaarVerificationService
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
    getDistances' <- parseField "getDistances" getDistances getDistancesPercentage
    getEstimatedPickupDistances' <- parseField "getEstimatedPickupDistances" getEstimatedPickupDistances getEstimatedPickupDistancesPercentage
    getRoutes' <- parseField "getRoutes" getRoutes getRoutesPercentage
    getPickupRoutes' <- parseField "getPickupRoutes" getPickupRoutes getPickupRoutesPercentage
    getTripRoutes' <- parseField "getTripRoutes" getTripRoutes getTripRoutesPercentage
    snapToRoad' <- parseField "snapToRoad" snapToRoad snapToRoadPercentage
    getPlaceName' <- parseField "getPlaceName" getPlaceName getPlaceNamePercentage
    getPlaceDetails' <- parseField "getPlaceDetails" getPlaceDetails getPlaceDetailsPercentage
    autoComplete' <- parseField "autoComplete" autoComplete autoCompletePercentage
    getDistancesForCancelRide' <- parseField "getDistancesForCancelRide" getDistancesForCancelRide getDistancesForCancelRidePercentage
    return $
      Domain.MerchantServiceUsageConfig
        { merchantId = fromKey merchantId,
          smsProvidersPriorityList = unPostgresList smsProvidersPriorityList,
          whatsappProvidersPriorityList = unPostgresList whatsappProvidersPriorityList,
          getDistances = getDistances',
          getEstimatedPickupDistances = getEstimatedPickupDistances',
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
      parseField fieldName field fieldPercentage = do
        mapsServiceUsagePercentage <- decodeFromText fieldPercentage & fromMaybeM (InternalError $ "Unable to decode MerchantServiceUsageConfigT." <> fieldName <> "Percentage")
        pure $ mkMapsServiceUsage field mapsServiceUsagePercentage

-- should we create separate table for percentage instead of using json?
instance ToTType MerchantServiceUsageConfigT Domain.MerchantServiceUsageConfig where
  toTType Domain.MerchantServiceUsageConfig {..} = do
    let mkPercentage = encodeToText . mkMapsServiceUsagePercentage
    MerchantServiceUsageConfigT
      { merchantId = toKey merchantId,
        smsProvidersPriorityList = PostgresList smsProvidersPriorityList,
        whatsappProvidersPriorityList = PostgresList whatsappProvidersPriorityList,
        getDistances = getDistances.mapsService,
        getDistancesPercentage = mkPercentage getDistances,
        getEstimatedPickupDistances = getEstimatedPickupDistances.mapsService,
        getEstimatedPickupDistancesPercentage = mkPercentage getEstimatedPickupDistances,
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
