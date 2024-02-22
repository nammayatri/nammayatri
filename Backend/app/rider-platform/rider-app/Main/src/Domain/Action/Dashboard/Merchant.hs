{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.Merchant
  ( mapsServiceConfigUpdate,
    mapsServiceUsageConfigUpdate,
    merchantUpdate,
    serviceUsageConfig,
    smsServiceConfigUpdate,
    smsServiceUsageConfigUpdate,
    createMerchantOperatingCity,
  )
where

import qualified "dashboard-helper-api" Dashboard.RiderPlatform.Merchant as Common
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Geometry as DGEO
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantMessage as DMM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Merchant.MerchantServiceConfig as DMSC
import qualified Domain.Types.Merchant.MerchantServiceUsageConfig as DMSUC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RiderConfig as DRC
import Environment
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.SMS as SMS
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Validation
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CQMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import qualified Storage.Queries.Geometry as QGEO
import Tools.Error

---------------------------------------------------------------------
merchantUpdate :: ShortId DM.Merchant -> Context.City -> Common.MerchantUpdateReq -> Flow APISuccess
merchantUpdate merchantShortId city req = do
  runRequestValidation Common.validateMerchantUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)

  let updMerchant =
        merchant{DM.name = fromMaybe merchant.name req.name,
                 DM.gatewayUrl = fromMaybe merchant.gatewayUrl req.gatewayUrl,
                 DM.registryUrl = fromMaybe merchant.registryUrl req.registryUrl
                }
  now <- getCurrentTime

  mbAllExophones <- forM req.exoPhones $ \exophones -> do
    allExophones <- CQExophone.findAllExophones
    let alreadyUsedPhones = getAllPhones $ filter (\exophone -> exophone.merchantOperatingCityId /= merchantOperatingCity.id) allExophones
    let reqPhones = getAllPhones $ toList exophones
    let busyPhones = filter (`elem` alreadyUsedPhones) reqPhones
    unless (null busyPhones) $ do
      throwError $ InvalidRequest $ "Next phones are already in use: " <> show busyPhones
    pure allExophones

  void $ CQM.update updMerchant
  whenJust req.exoPhones \exophones -> do
    CQExophone.deleteByMerchantOperatingCityId merchantOperatingCity.id
    forM_ exophones $ \exophoneReq -> do
      exophone <- buildExophone merchant.id merchantOperatingCity.id now exophoneReq
      CQExophone.create exophone

  CQM.clearCache updMerchant
  whenJust mbAllExophones $ \allExophones -> do
    let oldExophones = filter (\exophone -> exophone.merchantOperatingCityId == merchantOperatingCity.id) allExophones
    CQExophone.clearCache merchantOperatingCity.id oldExophones
  logTagInfo "dashboard -> merchantUpdate : " (show merchant.id)
  pure Success
  where
    getAllPhones es = (es <&> (.primaryPhone)) <> (es <&> (.backupPhone))

buildExophone :: MonadGuid m => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> UTCTime -> Common.ExophoneReq -> m DExophone.Exophone
buildExophone merchantId merchantOperatingCityId now req = do
  uid <- generateGUID
  pure
    DExophone.Exophone
      { id = uid,
        merchantId,
        merchantOperatingCityId,
        primaryPhone = req.primaryPhone,
        backupPhone = req.backupPhone,
        isPrimaryDown = False,
        callService = req.callService,
        updatedAt = now,
        createdAt = now
      }

---------------------------------------------------------------------
serviceUsageConfig ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow Common.ServiceUsageConfigRes
serviceUsageConfig merchantShortId city = do
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)
  config <- CQMSUC.findByMerchantOperatingCityId merchantOperatingCity.id >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCity.id.getId)
  pure $ mkServiceUsageConfigRes config

mkServiceUsageConfigRes :: DMSUC.MerchantServiceUsageConfig -> Common.ServiceUsageConfigRes
mkServiceUsageConfigRes DMSUC.MerchantServiceUsageConfig {..} =
  Common.ServiceUsageConfigRes
    { getEstimatedPickupDistances = Nothing,
      getPickupRoutes = Just getPickupRoutes,
      getTripRoutes = Just getTripRoutes,
      snapToRoadProvidersList = [],
      ..
    }

---------------------------------------------------------------------
mapsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceConfigUpdateReq ->
  Flow APISuccess
mapsServiceConfigUpdate merchantShortId _ req = do
  merchant <- findMerchantByShortId merchantShortId
  let serviceName = DMSC.MapsService $ Common.getMapsServiceFromReq req
  serviceConfig <- DMSC.MapsServiceConfig <$> Common.buildMapsServiceConfig req
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig
  _ <- CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id serviceName
  logTagInfo "dashboard -> mapsServiceConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
smsServiceConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceConfigUpdateReq ->
  Flow APISuccess
smsServiceConfigUpdate merchantShortId _ req = do
  merchant <- findMerchantByShortId merchantShortId
  let serviceName = DMSC.SmsService $ Common.getSmsServiceFromReq req
  serviceConfig <- DMSC.SmsServiceConfig <$> Common.buildSmsServiceConfig req
  merchantServiceConfig <- DMSC.buildMerchantServiceConfig merchant.id serviceConfig
  _ <- CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id serviceName
  logTagInfo "dashboard -> smsServiceConfigUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
mapsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceUsageConfigUpdateReq ->
  Flow APISuccess
mapsServiceUsageConfigUpdate merchantShortId city req = do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  whenJust req.getEstimatedPickupDistances $ \_ ->
    throwError (InvalidRequest "getEstimatedPickupDistances is not allowed for bap")
  merchant <- findMerchantByShortId merchantShortId

  forM_ Maps.availableMapsServices $ \service -> do
    when (Common.mapsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantIdAndService merchant.id (DMSC.MapsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for maps service " <> show service <> " is not provided")

  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOperatingCityId merchantOperatingCity.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCity.id.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{getDistances = fromMaybe merchantServiceUsageConfig.getDistances req.getDistances,
                                   getRoutes = fromMaybe merchantServiceUsageConfig.getRoutes req.getRoutes,
                                   snapToRoad = fromMaybe merchantServiceUsageConfig.snapToRoad req.snapToRoad,
                                   getPlaceName = fromMaybe merchantServiceUsageConfig.getPlaceName req.getPlaceName,
                                   getPlaceDetails = fromMaybe merchantServiceUsageConfig.getPlaceDetails req.getPlaceDetails,
                                   autoComplete = fromMaybe merchantServiceUsageConfig.autoComplete req.autoComplete
                                  }
  _ <- CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchantOperatingCity.id
  logTagInfo "dashboard -> mapsServiceUsageConfigUpdate : " (show merchantOperatingCity.id)
  pure Success

---------------------------------------------------------------------
smsServiceUsageConfigUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceUsageConfigUpdateReq ->
  Flow APISuccess
smsServiceUsageConfigUpdate merchantShortId city req = do
  runRequestValidation Common.validateSmsServiceUsageConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId

  forM_ SMS.availableSmsServices $ \service -> do
    when (Common.smsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantIdAndService merchant.id (DMSC.SmsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for sms service " <> show service <> " is not provided")

  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOperatingCityId merchantOperatingCity.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCity.id.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{smsProvidersPriorityList = req.smsProvidersPriorityList
                                  }
  _ <- CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchantOperatingCity.id
  logTagInfo "dashboard -> smsServiceUsageConfigUpdate : " (show merchantOperatingCity.id)
  pure Success

createMerchantOperatingCity :: ShortId DM.Merchant -> Context.City -> Common.CreateMerchantOperatingCityReqT -> Flow Common.CreateMerchantOperatingCityRes
createMerchantOperatingCity merchantShortId city req = do
  merchant <- findMerchantByShortId merchantShortId
  baseOperatingCity <- CQMOC.findByMerchantIdAndCity merchant.id city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show city)
  let baseOperatingCityId = baseOperatingCity.id

  -- city
  newOperatingCity <- buildMerchantOperatingCity merchant.id

  -- merchant message
  merchantMessages <- CQMM.findAllByMerchantOpCityId baseOperatingCityId
  newMerchantMessages <- mapM (buildMerchantMessage newOperatingCity.id) merchantMessages

  -- merchant payment method
  merchantPaymentMethods <- CQMPM.findAllByMerchantOperatingCityId baseOperatingCityId
  newMerchantPaymentMethods <- mapM (buildMerchantPaymentMethod newOperatingCity.id) merchantPaymentMethods

  -- merchant service usage config
  merchantServiceUsageConfig <- CQMSUC.findByMerchantOperatingCityId baseOperatingCityId >>= fromMaybeM (InvalidRequest "Merchant Service Usage Config not found")
  newMerchantServiceUsageConfig <- buildMerchantServiceUsageConfig newOperatingCity.id merchantServiceUsageConfig

  -- merchant service config
  -- merchantServiceConfigs <- CQMSC.findAllMerchantOpCityId baseOperatingCityId
  -- newMerchantServiceConfigs <- mapM (buildMerchantServiceConfig newOperatingCity.id) merchantServiceConfigs

  -- rider_config
  riderConfig <- CQRC.findByMerchantOperatingCityId baseOperatingCityId >>= fromMaybeM (InvalidRequest "Transporter Config not found")
  newRiderConfig <- buildRiderConfig newOperatingCity.id riderConfig

  -- geometry
  geometry <- buildGeometry

  -- exophone
  exophones <- CQExophone.findAllByMerchantOperatingCityId baseOperatingCityId
  whenJust (listToMaybe exophones) $ \exophone -> do
    exophone' <- buildNewExophone newOperatingCity.id exophone
    CQExophone.create exophone'

  QGEO.create geometry
  CQMOC.create newOperatingCity
  mapM_ CQMM.create newMerchantMessages
  mapM_ CQMPM.create newMerchantPaymentMethods
  CQMSUC.create newMerchantServiceUsageConfig
  -- mapM_ CQMSC.create newMerchantServiceConfigs
  CQRC.create newRiderConfig

  when req.enableForMerchant $ do
    let newOrigin = updateGeoRestriction merchant.geofencingConfig.origin
        newDestination = updateGeoRestriction merchant.geofencingConfig.destination
    CQM.updateGeofencingConfig merchant.id newOrigin newDestination
    CQM.clearCache merchant

  pure $ Common.CreateMerchantOperatingCityRes newOperatingCity.id.getId
  where
    updateGeoRestriction = \case
      Unrestricted -> Unrestricted
      Regions regions -> Regions $ regions <> [(show req.city)]

    buildGeometry = do
      id <- generateGUID
      pure
        DGEO.Geometry
          { id,
            region = show req.city,
            state = req.state,
            city = req.city,
            geom = Just req.geom
          }

    buildMerchantOperatingCity merchantId = do
      id <- generateGUID
      now <- getCurrentTime
      pure
        DMOC.MerchantOperatingCity
          { id,
            merchantId,
            merchantShortId,
            lat = req.lat,
            long = req.long,
            city = req.city,
            state = req.state,
            createdAt = now,
            updatedAt = now
          }

    buildNewExophone newCityId DExophone.Exophone {..} = do
      newId <- generateGUID
      now <- getCurrentTime
      return $
        DExophone.Exophone
          { id = newId,
            merchantOperatingCityId = newCityId,
            primaryPhone = req.exophone,
            backupPhone = req.exophone,
            isPrimaryDown = False,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildMerchantMessage newCityId DMM.MerchantMessage {..} = do
      now <- getCurrentTime
      return $
        DMM.MerchantMessage
          { merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildMerchantPaymentMethod newCityId DMPM.MerchantPaymentMethod {..} = do
      newId <- generateGUID
      now <- getCurrentTime
      return $
        DMPM.MerchantPaymentMethod
          { id = newId,
            merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }

    buildMerchantServiceUsageConfig newCityId DMSUC.MerchantServiceUsageConfig {..} = do
      now <- getCurrentTime
      return $
        DMSUC.MerchantServiceUsageConfig
          { merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }

    {- Do it once service config on basis on city id is implemented -}
    -- buildMerchantServiceConfig newCityId DMSC.MerchantServiceConfig {..} = do
    --   now <- getCurrentTime
    --   return $
    --     DMSC.MerchantServiceConfig
    --       { merchantOperatingCityId = Just newCityId,
    --         createdAt = now,
    --         updatedAt = now,
    --         ..
    --       }

    buildRiderConfig newCityId DRC.RiderConfig {..} = do
      now <- getCurrentTime
      return $
        DRC.RiderConfig
          { merchantOperatingCityId = newCityId,
            createdAt = now,
            updatedAt = now,
            ..
          }
