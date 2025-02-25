{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.Dashboard.Merchant
  ( postMerchantServiceConfigMapsUpdate,
    postMerchantServiceUsageConfigMapsUpdate,
    postMerchantUpdate,
    getMerchantServiceUsageConfig,
    postMerchantServiceConfigSmsUpdate,
    postMerchantServiceUsageConfigSmsUpdate,
    postMerchantConfigOperatingCityCreate,
    postMerchantSpecialLocationUpsert,
    deleteMerchantSpecialLocationDelete,
    postMerchantSpecialLocationGatesUpsert,
    deleteMerchantSpecialLocationGatesDelete,
    buildMerchantServiceConfig,
    postMerchantConfigFailover,
    postMerchantTicketConfigUpsert,
  )
where

import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management.Merchant as Common
import Control.Applicative
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv
import Data.List.Extra (notNull)
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime)
import qualified Data.Vector as V
import qualified Domain.Types
import qualified Domain.Types.BecknConfig as DBC
import Domain.Types.BusinessHour
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.Geometry as DGEO
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.MerchantServiceUsageConfig as DMSUC
import qualified Domain.Types.RiderConfig as DRC
import Domain.Types.ServiceCategory
import Domain.Types.ServicePeopleCategory
import Domain.Types.TicketPlace
import Domain.Types.TicketService
import Environment
import qualified EulerHS.Language as L
import qualified "shared-services" IssueManagement.Common as ICommon
import qualified "shared-services" IssueManagement.Domain.Types.Issue.IssueConfig as DIConfig
import qualified "shared-services" IssueManagement.Storage.CachedQueries.Issue.IssueConfig as CQIssueConfig
import Kernel.External.Call (CallService (Exotel))
import qualified Kernel.External.Maps as Maps
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.SMS as SMS
import Kernel.Prelude
import Kernel.Storage.Esqueleto (runTransaction)
import qualified Kernel.Storage.Esqueleto.Transactionable as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (..))
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Types.Registry (SimpleLookupRequest (..), lookupRequestToRedisKey)
import qualified Kernel.Types.Registry.Subscriber as BecknSub
import Kernel.Types.TimeBound
import Kernel.Utils.Common
import qualified Kernel.Utils.Registry as Registry
import Kernel.Utils.Validation
import qualified Lib.Queries.GateInfo as QGI
import qualified Lib.Queries.GateInfoGeom as QGIG
import qualified Lib.Queries.SpecialLocation as QSL
import qualified Lib.Queries.SpecialLocationGeom as QSLG
import qualified Lib.Types.GateInfo as D
import qualified Lib.Types.SpecialLocation as SL
import qualified Lib.Yudhishthira.Types as LYT
import qualified Registry.Beckn.Interface as RegistryIF
import qualified Registry.Beckn.Interface.Types as RegistryT
import SharedLogic.Merchant (findMerchantByShortId)
import Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantMessage as CQMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as CQMSC
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CQMSUC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.BecknConfig as SQBC
import qualified Storage.Queries.BusinessHour as SQBH
import qualified Storage.Queries.BusinessHourExtra as SQBHE
import qualified Storage.Queries.Geometry as QGEO
import qualified Storage.Queries.Merchant as QM
import qualified Storage.Queries.MerchantServiceConfig as SQMSC
import qualified Storage.Queries.ServiceCategory as SQSC
import qualified Storage.Queries.ServiceCategoryExtra as SQSCE
import qualified Storage.Queries.ServicePeopleCategory as SQSPC
import qualified Storage.Queries.ServicePeopleCategoryExtra as SQSPCE
import qualified Storage.Queries.TicketPlace as SQTP
import qualified Storage.Queries.TicketService as SQTS
import Tools.Error
import qualified Tools.Payment as Payment

---------------------------------------------------------------------
postMerchantUpdate :: ShortId DM.Merchant -> Context.City -> Common.MerchantUpdateReq -> Flow APISuccess
postMerchantUpdate merchantShortId city req = do
  runRequestValidation Common.validateMerchantUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)
  now <- getCurrentTime

  let updMerchant =
        merchant{DM.name = fromMaybe merchant.name req.name,
                 DM.gatewayUrl = fromMaybe merchant.gatewayUrl req.gatewayUrl,
                 DM.registryUrl = fromMaybe merchant.registryUrl req.registryUrl
                }

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
        createdAt = now,
        enableAlternateNumber = Just False
      }

---------------------------------------------------------------------
getMerchantServiceUsageConfig ::
  ShortId DM.Merchant ->
  Context.City ->
  Flow Common.ServiceUsageConfigRes
getMerchantServiceUsageConfig merchantShortId city = do
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
buildMerchantServiceConfig ::
  MonadTime m =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DMSC.ServiceConfig ->
  m DMSC.MerchantServiceConfig
buildMerchantServiceConfig merchantId merchantOperatingCityId serviceConfig = do
  now <- getCurrentTime
  pure
    DMSC.MerchantServiceConfig
      { merchantId,
        serviceConfig,
        merchantOperatingCityId,
        updatedAt = now,
        createdAt = now
      }

postMerchantServiceConfigMapsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceConfigMapsUpdate merchantShortId city req = do
  merchant <- findMerchantByShortId merchantShortId
  let serviceName = DMSC.MapsService $ Common.getMapsServiceFromReq req
  serviceConfig <- DMSC.MapsServiceConfig <$> Common.buildMapsServiceConfig req
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  merchantServiceConfig <- buildMerchantServiceConfig merchant.id merchantOperatingCity.id serviceConfig
  _ <- CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id merchantOperatingCity.id serviceName
  logTagInfo "dashboard -> postMerchantServiceConfigMapsUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
postMerchantServiceConfigSmsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceConfigSmsUpdate merchantShortId city req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId city
      >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  let serviceName = DMSC.SmsService $ Common.getSmsServiceFromReq req
  serviceConfig <- DMSC.SmsServiceConfig <$> Common.buildSmsServiceConfig req
  merchantServiceConfig <- buildMerchantServiceConfig merchant.id merchantOperatingCity.id serviceConfig
  _ <- CQMSC.upsertMerchantServiceConfig merchantServiceConfig
  CQMSC.clearCache merchant.id merchantOperatingCity.id serviceName
  logTagInfo "dashboard -> postMerchantServiceConfigSmsUpdate : " (show merchant.id)
  pure Success

---------------------------------------------------------------------
postMerchantServiceUsageConfigMapsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.MapsServiceUsageConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceUsageConfigMapsUpdate merchantShortId city req = do
  runRequestValidation Common.validateMapsServiceUsageConfigUpdateReq req
  whenJust req.getEstimatedPickupDistances $ \_ ->
    throwError (InvalidRequest "getEstimatedPickupDistances is not allowed for bap")
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  forM_ Maps.availableMapsServices $ \service -> do
    when (Common.mapsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantOpCityIdAndService merchant.id merchantOperatingCity.id (DMSC.MapsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for maps service " <> show service <> " is not provided")

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
  logTagInfo "dashboard -> postMerchantServiceUsageConfigMapsUpdate : " (show merchantOperatingCity.id)
  pure Success

---------------------------------------------------------------------
postMerchantServiceUsageConfigSmsUpdate ::
  ShortId DM.Merchant ->
  Context.City ->
  Common.SmsServiceUsageConfigUpdateReq ->
  Flow APISuccess
postMerchantServiceUsageConfigSmsUpdate merchantShortId city req = do
  runRequestValidation Common.validateSmsServiceUsageConfigUpdateReq req
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound ("merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city))
  forM_ SMS.availableSmsServices $ \service -> do
    when (Common.smsServiceUsedInReq req service) $ do
      void $
        CQMSC.findByMerchantOpCityIdAndService merchant.id merchantOperatingCity.id (DMSC.SmsService service)
          >>= fromMaybeM (InvalidRequest $ "Merchant config for sms service " <> show service <> " is not provided")

  merchantServiceUsageConfig <-
    CQMSUC.findByMerchantOperatingCityId merchantOperatingCity.id
      >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCity.id.getId)
  let updMerchantServiceUsageConfig =
        merchantServiceUsageConfig{smsProvidersPriorityList = req.smsProvidersPriorityList
                                  }
  _ <- CQMSUC.updateMerchantServiceUsageConfig updMerchantServiceUsageConfig
  CQMSUC.clearCache merchantOperatingCity.id
  logTagInfo "dashboard -> postMerchantServiceUsageConfigSmsUpdate : " (show merchantOperatingCity.id)
  pure Success

postMerchantSpecialLocationUpsert :: ShortId DM.Merchant -> Context.City -> Maybe (Id SL.SpecialLocation) -> Common.UpsertSpecialLocationReqT -> Flow APISuccess
postMerchantSpecialLocationUpsert merchantShortId _city mbSpecialLocationId request = do
  existingSLWithGeom <- maybe (return Nothing) (Esq.runInReplica . QSL.findByIdWithGeom) mbSpecialLocationId
  let mbExistingSL = fst <$> existingSLWithGeom
      mbGeom = snd =<< existingSLWithGeom
  updatedSL <- mkSpecialLocation mbExistingSL mbGeom
  void $
    runTransaction $
      if isJust mbExistingSL then QSLG.updateSpecialLocation updatedSL else QSLG.create updatedSL
  return Success
  where
    mkSpecialLocation :: Maybe SL.SpecialLocation -> Maybe Text -> Flow SL.SpecialLocation
    mkSpecialLocation mbExistingSpLoc mbGeometry = do
      let geom = request.geom <|> mbGeometry
      id <- maybe generateGUID (return . (.id)) mbExistingSpLoc
      now <- getCurrentTime
      (merchantOperatingCityId, merchantId) <- case request.city of
        Just opCity -> do
          merchantOperatingCity <-
            CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
              >>= fromMaybeM (MerchantOperatingCityDoesNotExist $ "merchantShortId: " <> merchantShortId.getShortId <> ", opCity: " <> show opCity)
          let merchantOperatingCityId = cast @DMOC.MerchantOperatingCity @SL.MerchantOperatingCity merchantOperatingCity.id
              merchantId = cast @DM.Merchant @SL.Merchant merchantOperatingCity.merchantId
          pure (merchantOperatingCityId, merchantId)
        Nothing -> case (mbExistingSpLoc >>= (.merchantOperatingCityId), mbExistingSpLoc >>= (.merchantId)) of
          (Just merchantOperatingCityId, Just merchantId) -> pure (merchantOperatingCityId, merchantId)
          (Just merchantOperatingCityId, Nothing) -> do
            merchantOperatingCity <-
              CQMOC.findById (cast @SL.MerchantOperatingCity @DMOC.MerchantOperatingCity merchantOperatingCityId)
                >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantOperatingCityId-" <> merchantOperatingCityId.getId)
            let merchantId = cast @DM.Merchant @SL.Merchant merchantOperatingCity.merchantId
            pure (merchantOperatingCityId, merchantId)
          (Nothing, _) -> throwError (InvalidRequest "Valid city should be provided")
      locationName <-
        fromMaybeM (InvalidRequest "Location Name cannot be empty for a new special location") $
          request.locationName <|> (mbExistingSpLoc <&> (.locationName))
      category <- fromMaybeM (InvalidRequest "Category is a required field for a new special location") $ request.category <|> (mbExistingSpLoc <&> (.category))
      return $
        SL.SpecialLocation
          { gates = [],
            createdAt = maybe now (.createdAt) mbExistingSpLoc,
            updatedAt = now,
            merchantOperatingCityId = Just merchantOperatingCityId,
            linkedLocationsIds = maybe [] (.linkedLocationsIds) mbExistingSpLoc,
            locationType = SL.Closed,
            merchantId = Just merchantId,
            ..
          }

deleteMerchantSpecialLocationDelete :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Flow APISuccess
deleteMerchantSpecialLocationDelete _merchantShortid _city specialLocationId = do
  void $ QSL.findById specialLocationId >>= fromMaybeM (InvalidRequest "Special Location with given id not found")
  void $ runTransaction $ QSL.deleteById specialLocationId
  void $ runTransaction $ QGI.deleteAll specialLocationId
  pure Success

postMerchantSpecialLocationGatesUpsert :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Common.UpsertSpecialLocationGateReqT -> Flow APISuccess
postMerchantSpecialLocationGatesUpsert _merchantShortId _city specialLocationId request = do
  specialLocation <- QSL.findById specialLocationId >>= fromMaybeM (InvalidRequest "Cound not find a special location with the provided id")
  existingGates <- QGI.findAllGatesBySpecialLocationId specialLocationId
  createOrUpdateGate specialLocation existingGates request
  return Success
  where
    createOrUpdateGate :: SL.SpecialLocation -> [(D.GateInfo, Maybe Text)] -> Common.UpsertSpecialLocationGateReqT -> Flow ()
    createOrUpdateGate specialLocation existingGates req = do
      let existingGatewithGeom = find (\(gate, _mbGeom) -> normalizeName gate.name == normalizeName req.name) existingGates
          existingGate = fst <$> existingGatewithGeom
          mbGeom = snd =<< existingGatewithGeom
      updatedGate <- mkGate specialLocation req existingGate mbGeom
      void $
        runTransaction $
          if isNothing existingGate then QGIG.create updatedGate else QGIG.updateGate updatedGate

    mkGate :: SL.SpecialLocation -> Common.UpsertSpecialLocationGateReqT -> Maybe D.GateInfo -> Maybe Text -> Flow D.GateInfo
    mkGate specialLocation reqT mbGate mbGeom = do
      id <- cast <$> maybe generateGUID (return . (.id)) mbGate
      now <- getCurrentTime
      latitude <- fromMaybeM (InvalidRequest "Latitude field cannot be empty for a new gate") $ reqT.latitude <|> (mbGate <&> (.point.lat))
      longitude <- fromMaybeM (InvalidRequest "Longitude field cannot be empty for a new gate") $ reqT.longitude <|> (mbGate <&> (.point.lon))
      address <- fromMaybeM (InvalidRequest "Address cannot be empty for a new gate") $ reqT.address <|> (mbGate >>= (.address))
      let canQueueUpOnGate = fromMaybe False $ reqT.canQueueUpOnGate <|> (mbGate <&> (.canQueueUpOnGate))
          defaultDriverExtra = reqT.defaultDriverExtra <|> (mbGate >>= (.defaultDriverExtra))
          geom = reqT.geom <|> mbGeom
      return $
        D.GateInfo
          { name = reqT.name,
            address = Just address,
            geom,
            createdAt = maybe now (.createdAt) mbGate,
            updatedAt = now,
            point = LatLong {lat = latitude, lon = longitude},
            gateType = D.Pickup,
            merchantId = specialLocation.merchantId,
            merchantOperatingCityId = specialLocation.merchantOperatingCityId,
            ..
          }

deleteMerchantSpecialLocationGatesDelete :: ShortId DM.Merchant -> Context.City -> Id SL.SpecialLocation -> Text -> Flow APISuccess
deleteMerchantSpecialLocationGatesDelete _merchantShortId _city specialLocationId gateName = do
  existingGates <- QGI.findAllGatesBySpecialLocationId specialLocationId
  let existingGate = fst <$> find (\(gate, _mbGeom) -> normalizeName gate.name == normalizeName gateName) existingGates
  case existingGate of
    Nothing -> throwError $ InvalidRequest "Could not find any gates with the specified name for the given specialLocationId"
    Just gate -> runTransaction $ QGI.deleteById gate.id
  return Success

normalizeName :: Text -> Text
normalizeName = T.strip . T.toLower

postMerchantConfigOperatingCityCreate :: ShortId DM.Merchant -> Context.City -> Common.CreateMerchantOperatingCityReqT -> Flow Common.CreateMerchantOperatingCityRes
postMerchantConfigOperatingCityCreate merchantShortId city req = do
  when (req.city == Context.AnyCity) $ throwError $ InvalidRequest "This Operation is not Allowed For AnyCity"
  baseMerchant <- findMerchantByShortId merchantShortId
  baseOperatingCity <- CQMOC.findByMerchantIdAndCity baseMerchant.id city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> baseMerchant.id.getId <> "-city-" <> show city)
  now <- getCurrentTime
  let baseMerchantId = baseMerchant.id
      baseOperatingCityId = baseOperatingCity.id

  let newMerchantId =
        case req.merchantData of
          Just merchantData -> Id merchantData.subscriberId
          Nothing -> baseMerchantId

  -- merchant
  mbNewMerchant <-
    case req.merchantData of
      Just merchantData -> do
        CQM.findById newMerchantId >>= \case
          Nothing -> do
            merchant <- CQM.findById baseMerchantId >>= fromMaybeM (InvalidRequest "Base Merchant not found")
            let newMerchant = buildMerchant newMerchantId merchantData now merchant
            return $ Just newMerchant
          _ -> return Nothing
      _ -> return Nothing

  cityAlreadyCreated <- CQMOC.findByMerchantIdAndCity newMerchantId req.city
  newMerchantOperatingCityId <-
    case cityAlreadyCreated of
      Just newCity -> return newCity.id
      Nothing -> generateGUID

  let newMerchantShortId = maybe merchantShortId (.shortId) mbNewMerchant
  -- city
  let mbNewOperatingCity =
        case cityAlreadyCreated of
          Nothing -> do
            let newOperatingCity = buildMerchantOperatingCity newMerchantId newMerchantOperatingCityId now newMerchantShortId
            Just newOperatingCity
          _ -> Nothing

  -- merchant message
  mbMerchantMessages <-
    CQMM.findAllByMerchantOpCityIdInRideFlow newMerchantOperatingCityId [] >>= \case
      [] -> do
        merchantMessages <- CQMM.findAllByMerchantOpCityId baseOperatingCityId Nothing
        let newMerchantMessages = map (buildMerchantMessage newMerchantId newMerchantOperatingCityId now) merchantMessages
        return $ Just newMerchantMessages
      _ -> return Nothing -- ignore

  -- merchant payment method
  mbMerchantPaymentMethods <-
    CQMPM.findAllByMerchantOperatingCityId newMerchantOperatingCityId >>= \case
      [] -> do
        merchantPaymentMethods <- CQMPM.findAllByMerchantOperatingCityId baseOperatingCityId
        newMerchantPaymentMethods <- mapM (buildMerchantPaymentMethod newMerchantId newMerchantOperatingCityId now) merchantPaymentMethods
        return $ Just newMerchantPaymentMethods
      _ -> return Nothing

  -- merchant service usage config
  mbMerchantServiceUsageConfig <-
    CQMSUC.findByMerchantOperatingCityId newMerchantOperatingCityId >>= \case
      Nothing -> do
        merchantServiceUsageConfig <- CQMSUC.findByMerchantOperatingCityId baseOperatingCityId >>= fromMaybeM (InvalidRequest "Merchant Service Usage Config not found")
        let newMerchantServiceUsageConfig = buildMerchantServiceUsageConfig newMerchantId newMerchantOperatingCityId now merchantServiceUsageConfig
        return $ Just newMerchantServiceUsageConfig
      _ -> return Nothing

  -- merchant service config
  mbMerchantServiceConfig <-
    SQMSC.findAllByMerchantOperatingCityId newMerchantOperatingCityId >>= \case
      [] -> do
        merchantServiceConfigs <- SQMSC.findAllByMerchantOperatingCityId baseOperatingCityId
        let newMerchantServiceConfigs = map (buildMerchantServiceConfigs newMerchantId newMerchantOperatingCityId now) merchantServiceConfigs
        return $ Just newMerchantServiceConfigs
      _ -> return Nothing

  -- rider_config
  mbRiderConfig <- do
    let baseVersion = LYT.ConfigVersionMap {version = 1, config = LYT.RIDER_CONFIG LYT.RiderConfig}
    QRC.findByMerchantOperatingCityId newMerchantOperatingCityId (Just [baseVersion]) >>= \case
      Nothing -> do
        riderConfig <- QRC.findByMerchantOperatingCityId baseOperatingCityId (Just [baseVersion]) >>= fromMaybeM (InvalidRequest "Rider Config not found")
        let newRiderConfig = buildRiderConfig newMerchantId newMerchantOperatingCityId now riderConfig
        return $ Just newRiderConfig
      _ -> return Nothing

  -- geometry
  mbGeometry <-
    QGEO.findGeometryByStateAndCity req.city req.state >>= \case
      Nothing -> do
        Just <$> buildGeometry
      _ -> return Nothing

  -- exophone
  mbExophone <-
    CQExophone.findAllByMerchantOperatingCityId newMerchantOperatingCityId >>= \case
      [] -> do
        exophones <- CQExophone.findAllByMerchantOperatingCityId baseOperatingCityId
        return $ Just exophones
      _ -> return Nothing

  -- issue config
  mbIssueConfig <-
    CQIssueConfig.findByMerchantOpCityId (cast newMerchantOperatingCityId) ICommon.CUSTOMER >>= \case
      Nothing -> do
        issueConfig <- CQIssueConfig.findByMerchantOpCityId (cast baseOperatingCityId) ICommon.CUSTOMER >>= fromMaybeM (InvalidRequest "Issue Config not found")
        newIssueConfig <- buildIssueConfig newMerchantId newMerchantOperatingCityId now issueConfig
        return $ Just newIssueConfig
      _ -> return Nothing

  -- beckn config
  mbBecknConfig <-
    SQBC.findAllByMerchantOperatingCityId (Just newMerchantOperatingCityId) >>= \case
      [] -> do
        becknConfig <- SQBC.findAllByMerchantOperatingCityId (Just baseOperatingCityId)
        newBecknConfig <- mapM (buildBecknConfig newMerchantId newMerchantOperatingCityId now) becknConfig
        return $ Just newBecknConfig
      _ -> return Nothing

  nyRegistryUrl <- asks (.nyRegistryUrl)
  let uniqueKeyId = baseMerchant.bapUniqueKeyId
      subscriberId = baseMerchant.bapId
      subType = BecknSub.BAP
      domain = Context.MOBILITY
      lookupReq = SimpleLookupRequest {unique_key_id = uniqueKeyId, subscriber_id = subscriberId, merchant_id = baseMerchant.id.getId, subscriber_type = subType, ..}
  mbAddCityReq <-
    Registry.registryLookup nyRegistryUrl lookupReq >>= \case
      Nothing -> do
        logError $ "No entry found for subscriberId: " <> subscriberId <> ", uniqueKeyId: " <> uniqueKeyId <> " in NY registry"
        return Nothing
      Just sub | req.city `elem` sub.city -> return Nothing
      Just _ -> Just <$> RegistryT.buildAddCityNyReq (req.city :| []) uniqueKeyId subscriberId subType domain

  finally
    ( do
        whenJust mbGeometry $ \geometry -> QGEO.create geometry
        whenJust mbNewMerchant $ \newMerchant -> QM.create newMerchant
        whenJust mbNewOperatingCity $ \newOperatingCity -> CQMOC.create newOperatingCity
        whenJust mbMerchantMessages $ \merchantMessages -> mapM_ CQMM.create merchantMessages
        whenJust mbMerchantPaymentMethods $ \mPM -> mapM_ CQMPM.create mPM
        whenJust mbMerchantServiceUsageConfig $ \mSUC -> CQMSUC.create mSUC
        whenJust mbMerchantServiceConfig $ \merchantServiceConfigs -> mapM_ SQMSC.create merchantServiceConfigs
        whenJust mbBecknConfig $ \becknConfig -> mapM_ SQBC.create becknConfig
        whenJust mbRiderConfig $ \riderConfig -> QRC.create riderConfig

        whenJust mbExophone $ \exophones -> do
          whenJust (find (\ex -> ex.callService == Exotel) exophones) $ \exophone -> do
            exophone' <- buildNewExophone newMerchantId newMerchantOperatingCityId now exophone
            CQExophone.create exophone'
        whenJust mbIssueConfig $ \issueConfig -> CQIssueConfig.create issueConfig

        when (req.enableForMerchant) $ do
          let origin = maybe baseMerchant.geofencingConfig.origin (.geofencingConfig.origin) mbNewMerchant
              destination = maybe baseMerchant.geofencingConfig.destination (.geofencingConfig.destination) mbNewMerchant
              newOrigin = updateGeoRestriction origin
              newDestination = updateGeoRestriction destination

          when (checkGeofencingConfig origin && checkGeofencingConfig destination) $ do
            CQM.updateGeofencingConfig newMerchantId newOrigin newDestination
            CQM.clearCache $ fromMaybe baseMerchant mbNewMerchant

        whenJust mbAddCityReq $ \addCityReq ->
          void $ RegistryIF.updateSubscriber addCityReq
    )
    ( do
        CQMM.clearCacheById newMerchantOperatingCityId
        CQMPM.clearCache newMerchantOperatingCityId
        CQMSUC.clearCache newMerchantOperatingCityId
        QRC.clearCache newMerchantOperatingCityId
        CQIssueConfig.clearIssueConfigCache (cast newMerchantOperatingCityId) ICommon.CUSTOMER
        exoPhone <- CQExophone.findAllByMerchantOperatingCityId newMerchantOperatingCityId
        CQExophone.clearCache newMerchantOperatingCityId exoPhone
        whenJust mbAddCityReq $ \_ -> Redis.del $ cacheRegistryKey <> lookupRequestToRedisKey lookupReq
    )
  pure $ Common.CreateMerchantOperatingCityRes newMerchantOperatingCityId.getId
  where
    updateGeoRestriction = \case
      Unrestricted -> Unrestricted
      Regions regions -> Regions $ regions <> [show req.city]
    checkGeofencingConfig = \case
      Regions regions -> notElem (show req.city) regions
      Unrestricted -> True

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

    buildMerchant merchantId merchantData currentTime DM.Merchant {..} = do
      DM.Merchant
        { id = merchantId,
          subscriberId = ShortId merchantData.shortId,
          shortId = ShortId merchantData.shortId,
          fallbackShortId = ShortId merchantData.shortId,
          name = merchantData.name,
          defaultCity = req.city,
          defaultState = req.state,
          country = req.country,
          geofencingConfig =
            GeofencingConfig
              { origin = Regions [show req.city],
                destination = Regions [show req.city]
              },
          bapId = T.replace id.getId merchantId.getId bapId,
          bapUniqueKeyId = merchantData.uniqueKeyId,
          driverOfferMerchantId = merchantData.networkParticipantId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildMerchantOperatingCity newMerchantId cityId currentTime newMerchantShortId = do
      DMOC.MerchantOperatingCity
        { id = cityId,
          merchantId = newMerchantId,
          merchantShortId = newMerchantShortId,
          lat = req.lat,
          long = req.long,
          city = req.city,
          state = req.state,
          country = req.country,
          distanceUnit = fromMaybe Meter req.distanceUnit,
          createdAt = currentTime,
          updatedAt = currentTime
        }

    buildNewExophone mId newCityId currentTime DExophone.Exophone {..} = do
      newId <- generateGUID
      return
        DExophone.Exophone
          { id = newId,
            merchantOperatingCityId = newCityId,
            merchantId = mId,
            primaryPhone = req.exophone,
            backupPhone = req.exophone,
            isPrimaryDown = False,
            createdAt = currentTime,
            updatedAt = currentTime,
            ..
          }

    buildMerchantMessage mId newCityId currentTime DMM.MerchantMessage {..} =
      DMM.MerchantMessage
        { merchantOperatingCityId = newCityId,
          merchantId = mId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildMerchantPaymentMethod mId newCityId currentTime DMPM.MerchantPaymentMethod {..} = do
      newId <- generateGUID
      return
        DMPM.MerchantPaymentMethod
          { id = newId,
            merchantId = mId,
            merchantOperatingCityId = newCityId,
            createdAt = currentTime,
            updatedAt = currentTime,
            ..
          }

    buildMerchantServiceUsageConfig mId newCityId currentTime DMSUC.MerchantServiceUsageConfig {..} = do
      DMSUC.MerchantServiceUsageConfig
        { merchantOperatingCityId = newCityId,
          merchantId = mId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    {- Do it once service config on basis on city id is implemented -}
    buildMerchantServiceConfigs mId newCityId currentTime DMSC.MerchantServiceConfig {..} =
      DMSC.MerchantServiceConfig
        { merchantOperatingCityId = newCityId,
          merchantId = mId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildRiderConfig mId newCityId currentTime DRC.RiderConfig {..} =
      DRC.RiderConfig
        { merchantOperatingCityId = newCityId,
          merchantId = Just mId,
          createdAt = currentTime,
          updatedAt = currentTime,
          ..
        }

    buildIssueConfig mId newCityId currentTime DIConfig.IssueConfig {..} = do
      newId <- generateGUID
      return
        DIConfig.IssueConfig
          { id = newId,
            merchantId = cast mId,
            merchantOperatingCityId = cast newCityId,
            createdAt = currentTime,
            updatedAt = currentTime,
            ..
          }

    buildBecknConfig newMerchantId newCityId currentTime DBC.BecknConfig {..} = do
      newId <- generateGUID
      let newSubscriberUrlText = maybe (showBaseUrl subscriberUrl) (\mId -> T.replace mId.getId newMerchantId.getId (showBaseUrl subscriberUrl)) merchantId
      newSubscriberUrl <- parseBaseUrl newSubscriberUrlText
      return
        DBC.BecknConfig
          { id = newId,
            merchantId = Just newMerchantId,
            merchantOperatingCityId = Just newCityId,
            subscriberId = maybe subscriberId (\mId -> T.replace mId.getId newMerchantId.getId subscriberId) merchantId,
            subscriberUrl = newSubscriberUrl,
            uniqueKeyId = fromMaybe uniqueKeyId (req.merchantData <&> (.uniqueKeyId)),
            createdAt = currentTime,
            updatedAt = currentTime,
            ..
          }

postMerchantConfigFailover :: ShortId DM.Merchant -> Context.City -> Common.ConfigNames -> Common.ConfigFailoverReq -> Flow APISuccess
postMerchantConfigFailover merchantShortId city configNames req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <- CQMOC.findByMerchantShortIdAndCity merchantShortId city >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show city)
  case configNames of
    Common.BecknNetwork -> do
      configureBecknNetworkFailover merchant req
    _ -> do
      configureMessageProviderFailover merchantOperatingCity req
  pure Success

configureBecknNetworkFailover :: DM.Merchant -> Common.ConfigFailoverReq -> Flow ()
configureBecknNetworkFailover merchant req = do
  case req.priorityOrder of
    Just priorityOrder -> do
      CQM.updateGatewayAndRegistryPriorityList merchant (castNetworkEnums <$> priorityOrder.networkTypes)
    Nothing -> do
      let networkPriorityList = reorderList merchant.gatewayAndRegistryPriorityList
      CQM.updateGatewayAndRegistryPriorityList merchant networkPriorityList
  pure ()

configureMessageProviderFailover :: DMOC.MerchantOperatingCity -> Common.ConfigFailoverReq -> Flow ()
configureMessageProviderFailover merchantOperatingCity req = do
  case req.priorityOrder of
    Just priorityOrder -> do
      when (notNull priorityOrder.smsProviders) do CQMSUC.updateSmsProvidersPriorityList priorityOrder.smsProviders merchantOperatingCity.id
      when (notNull priorityOrder.whatsappProviders) do CQMSUC.updateWhatsappProvidersPriorityList priorityOrder.whatsappProviders merchantOperatingCity.id
    Nothing -> do
      merchantServiceUsageConfig <- CQMSUC.findByMerchantOperatingCityId merchantOperatingCity.id >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantOperatingCity.id.getId)
      let messageProviderPriorityList = reorderList merchantServiceUsageConfig.smsProvidersPriorityList
          whatsappProviderPriorityList = reorderList merchantServiceUsageConfig.whatsappProvidersPriorityList
      CQMSUC.updateSmsProvidersPriorityList messageProviderPriorityList merchantOperatingCity.id
      CQMSUC.updateWhatsappProvidersPriorityList whatsappProviderPriorityList merchantOperatingCity.id
  pure ()

reorderList :: [a] -> [a]
reorderList [] = []
reorderList (x : xs) = xs ++ [x]

castNetworkEnums :: Common.NetworkEnums -> Domain.Types.GatewayAndRegistryService
castNetworkEnums Common.ONDC = Domain.Types.ONDC
castNetworkEnums Common.NY = Domain.Types.NY

---------------------------------------------------------------------
data TicketConfigCSVRow = TicketConfigCSVRow
  { name :: Text,
    city :: Text,
    allowSameDayBooking :: Text,
    description :: Text,
    openTimings :: Text,
    closeTimings :: Text,
    placeType :: Text,
    shortDesc :: Text,
    gallery :: Text,
    iconUrl :: Text,
    lat :: Text,
    lon :: Text,
    mapImageUrl :: Text,
    status :: Text,
    termsAndConditions :: Text,
    termsAndConditionsUrl :: Text,
    svc :: Text,
    svcShortDesc :: Text,
    svcOperationalDays :: Text,
    svcStartDate :: Text,
    svcEndDate :: Text,
    svcMaxVerification :: Text,
    svcExpiryType :: Text,
    svcExpiryValue :: Text,
    svcExpiryTime :: Text,
    svcAllowFutureBooking :: Text,
    svcAllowCancellation :: Text,
    businessHourType :: Text,
    businessHourSlotTime :: Text,
    businessHourStartTime :: Text,
    businessHourEndTime :: Text,
    svcCategoryAllowedSeats :: Text,
    svcCategoryAvailableSeats :: Text,
    svcCategoryDescription :: Text,
    svcCategoryName :: Text,
    peopleCategoryName :: Text,
    peopleCategoryDescription :: Text,
    priceAmount :: Text,
    priceCurrency :: Text,
    pricingType :: Text,
    peakTimings :: Text,
    peakDays :: Text,
    cancellationType :: Text,
    cancellationTime :: Text,
    cancellationFee :: Text,
    vendorSplitDetails :: Text
  }
  deriving (Show)

instance FromNamedRecord TicketConfigCSVRow where
  parseNamedRecord r =
    TicketConfigCSVRow
      <$> r .: "name"
      <*> r .: "city"
      <*> r .: "allow_same_day_booking"
      <*> r .: "description"
      <*> r .: "open_timings"
      <*> r .: "close_timing"
      <*> r .: "place_type"
      <*> r .: "short_desc"
      <*> r .: "gallery"
      <*> r .: "icon_url"
      <*> r .: "lat"
      <*> r .: "lon"
      <*> r .: "map_image_url"
      <*> r .: "status"
      <*> r .: "terms_and_conditions"
      <*> r .: "terms_and_conditions_url"
      <*> r .: "svc"
      <*> r .: "svc_short_desc"
      <*> r .: "svc_operational_days"
      <*> r .: "svc_start_date"
      <*> r .: "svc_end_date"
      <*> r .: "svc_max_verification"
      <*> r .: "svc_expiry_type"
      <*> r .: "svc_expiry_value"
      <*> r .: "svc_expiry_time"
      <*> r .: "svc_allow_future_booking"
      <*> r .: "svc_allow_cancellation"
      <*> r .: "business_hour_type"
      <*> r .: "business_hour_slot_time"
      <*> r .: "business_hour_start_time"
      <*> r .: "business_hour_end_time"
      <*> r .: "svc_category_allowed_seats"
      <*> r .: "svc_category_available_seats"
      <*> r .: "svc_category_description"
      <*> r .: "svc_category_name"
      <*> r .: "people_category_name"
      <*> r .: "people_category_description"
      <*> r .: "price_amount"
      <*> r .: "price_currency"
      <*> r .: "pricing_type"
      <*> r .: "peak_timings"
      <*> r .: "peak_days"
      <*> r .: "cancellation_type"
      <*> r .: "cancellation_time"
      <*> r .: "cancellation_fee"
      <*> r .: "vendor_split_details"

postMerchantTicketConfigUpsert :: ShortId DM.Merchant -> Context.City -> Common.UpsertTicketConfigReq -> Flow Common.UpsertTicketConfigResp
postMerchantTicketConfigUpsert merchantShortId opCity request = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  flatTicketConfigs <- readCsv request.file merchantOpCity
  logTagInfo "Read file: " (show flatTicketConfigs)
  void $ (processTicketConfigGroup . groupTicketEntities) flatTicketConfigs
  return $
    Common.UpsertTicketConfigResp
      { unprocessedTicketConfigs = [], -- handle race condition and errors later if needed
        success = "Ticket configs updated successfully"
      }
  where
    cleanField :: Text -> Maybe Text
    cleanField txt = replaceEmpty (T.strip txt)

    cleanMaybeCSVField :: Int -> Text -> Text -> Maybe Text
    cleanMaybeCSVField _ fieldValue _ = cleanField fieldValue

    replaceEmpty :: Text -> Maybe Text
    replaceEmpty = \case
      "" -> Nothing
      "no constraint" -> Nothing
      "no_constraint" -> Nothing
      x -> Just x

    readCsv csvFile merchantOpCity = do
      csvData <- L.runIO $ BS.readFile csvFile
      case (decodeByName $ LBS.fromStrict csvData :: Either String (Header, V.Vector TicketConfigCSVRow)) of
        Left err -> throwError (InvalidRequest $ show err)
        Right (_, v) -> V.imapM (makeTicketConfigs merchantOpCity) v >>= (pure . V.toList)

    readCSVField :: Read a => Int -> Text -> Text -> Flow a
    readCSVField idx fieldValue fieldName =
      cleanField fieldValue >>= readMaybe . T.unpack & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

    readMaybeCSVField :: Read a => Int -> Text -> Text -> Maybe a
    readMaybeCSVField _ fieldValue _ =
      cleanField fieldValue >>= readMaybe . T.unpack

    cleanCSVField :: Int -> Text -> Text -> Flow Text
    cleanCSVField idx fieldValue fieldName =
      cleanField fieldValue & fromMaybeM (InvalidRequest $ "Invalid " <> fieldName <> ": " <> show fieldValue <> " at row: " <> show idx)

    groupTicketEntities ::
      [(TicketPlace, TicketService, BusinessHour, ServiceCategory, ServicePeopleCategory)] ->
      [(TicketPlace, [(TicketService, [(BusinessHour, [(ServiceCategory, [ServicePeopleCategory])])])])]
    groupTicketEntities = foldl insertTuple []
      where
        insertTuple acc (place, service, businessHour, category, peopleCategory) =
          let accUpdated =
                case lookupPlace place acc of
                  Just (existingPlace, serviceList) ->
                    let updatedServices = insertService service serviceList businessHour category peopleCategory
                     in replacePlace (existingPlace, updatedServices) acc
                  Nothing -> (place, [(service, [(businessHour, [(category, [peopleCategory])])])]) : acc
           in accUpdated

        insertService service [] businessHour category peopleCategory = [(service, [(businessHour, [(category, [peopleCategory])])])]
        insertService service ((existingService, businessHourList) : rest) businessHour category peopleCategory =
          if existingService.id == service.id
            then (existingService, insertBusinessHour businessHour businessHourList category peopleCategory) : rest
            else (existingService, businessHourList) : insertService service rest businessHour category peopleCategory

        insertBusinessHour businessHour [] category peopleCategory = [(businessHour, [(category, [peopleCategory])])]
        insertBusinessHour businessHour ((existingBusinessHour, categoryList) : rest) category peopleCategory =
          if existingBusinessHour.id == businessHour.id
            then (existingBusinessHour, insertCategory category categoryList peopleCategory) : rest
            else (existingBusinessHour, categoryList) : insertBusinessHour businessHour rest category peopleCategory

        insertCategory category [] peopleCategory = [(category, [peopleCategory])]
        insertCategory category ((existingCategory, peopleCategoryList) : rest) peopleCategory =
          if existingCategory.id == category.id
            then (existingCategory, insertPeopleCategory peopleCategory peopleCategoryList) : rest
            else (existingCategory, peopleCategoryList) : insertCategory category rest peopleCategory

        insertPeopleCategory peopleCategory [] = [peopleCategory]
        insertPeopleCategory peopleCategory (existingPeopleCategory : rest) =
          if existingPeopleCategory.id == peopleCategory.id
            then existingPeopleCategory : rest
            else existingPeopleCategory : insertPeopleCategory peopleCategory rest

        lookupPlace ::
          TicketPlace ->
          [(TicketPlace, [(TicketService, [(BusinessHour, [(ServiceCategory, [ServicePeopleCategory])])])])] ->
          Maybe (TicketPlace, [(TicketService, [(BusinessHour, [(ServiceCategory, [ServicePeopleCategory])])])])
        lookupPlace place = find (\(p, _) -> p.id == place.id)

        replacePlace ::
          (TicketPlace, [(TicketService, [(BusinessHour, [(ServiceCategory, [ServicePeopleCategory])])])]) ->
          [(TicketPlace, [(TicketService, [(BusinessHour, [(ServiceCategory, [ServicePeopleCategory])])])])] ->
          [(TicketPlace, [(TicketService, [(BusinessHour, [(ServiceCategory, [ServicePeopleCategory])])])])]
        replacePlace (np, ns) = map (\(p, services) -> if p.id == np.id then (np, ns) else (p, services))

    processTicketConfigGroup :: [(TicketPlace, [(TicketService, [(BusinessHour, [(ServiceCategory, [ServicePeopleCategory])])])])] -> Flow ()
    processTicketConfigGroup = mapM_ upsertPlace
      where
        upsertPlace (place, serviceGroups) = do
          existingPlace <- SQTP.findByNameAndCity place.name place.merchantOperatingCityId
          case existingPlace of
            Just oldPlace -> do
              mapM_ (upsertService oldPlace.id.getId) serviceGroups
              SQTP.updateByPrimaryKey place{id = oldPlace.id}
            Nothing -> do
              newPlaceId <- generateGUID
              mapM_ (upsertService newPlaceId) serviceGroups
              SQTP.create place{id = Id newPlaceId}

        upsertService placeId (service, businessHourGroups) = do
          existingService <- SQTS.findByPlacesIdAndService placeId service.service
          let bHours = case existingService of
                Nothing -> []
                Just svc -> svc.businessHours
          processedBusinessHours <- mapM (upsertBusinessHour bHours) businessHourGroups
          case existingService of
            Nothing -> do
              newServiceId <- generateGUID
              SQTS.create service{id = newServiceId, placesId = placeId, businessHours = processedBusinessHours}
            Just existingSvc -> SQTS.updateByPrimaryKey service{placesId = placeId, id = existingSvc.id, businessHours = processedBusinessHours}

        upsertBusinessHour businessHourIds (businessHour, categoryGroups) = do
          existingBH <- SQBHE.findByBtypeAndId businessHour.btype businessHourIds
          let categoryIds = case existingBH of
                Nothing -> []
                Just bh -> bh.categoryId
          processedCategories <- mapM (upsertCategory categoryIds) categoryGroups
          case existingBH of
            Nothing -> do
              newBusinessHourId <- generateGUID
              SQBH.create businessHour {id = newBusinessHourId, categoryId = processedCategories}
              return newBusinessHourId
            Just bh -> do
              SQBH.updateByPrimaryKey businessHour{id = bh.id, categoryId = processedCategories}
              return bh.id

        upsertCategory categoryIds (category, peopleCategories) = do
          existingCategory <- SQSCE.findByIdAndName categoryIds category.name
          let peopleCategoryIds = case existingCategory of
                Nothing -> []
                Just cat -> cat.peopleCategory
          processedPeopleCategories <- mapM (upsertPeopleCategory peopleCategoryIds) peopleCategories
          case existingCategory of
            Nothing -> do
              newCategoryId <- generateGUID
              SQSC.create category {id = newCategoryId, peopleCategory = processedPeopleCategories}
              return newCategoryId
            Just sc -> do
              SQSC.updateByPrimaryKey category{id = sc.id, peopleCategory = processedPeopleCategories}
              return sc.id

        upsertPeopleCategory peopleCategoryIds peopleCategory = do
          existingPeopleCategory <- SQSPCE.findByIdAndName peopleCategoryIds peopleCategory.name
          case existingPeopleCategory of
            Nothing -> do
              newPeopleCategoryId <- generateGUID
              SQSPC.create peopleCategory{id = newPeopleCategoryId}
              return newPeopleCategoryId
            Just pc -> do
              SQSPC.updateByPrimaryKey peopleCategory{id = pc.id}
              return pc.id

    makeTicketConfigs :: DMOC.MerchantOperatingCity -> Int -> TicketConfigCSVRow -> Flow (TicketPlace, TicketService, BusinessHour, ServiceCategory, ServicePeopleCategory)
    makeTicketConfigs merchantOpCity idx row = do
      now <- getCurrentTime
      let createdAt = now
          updatedAt = now
          ticketPlaceId = Id (show row.name <> "_" <> merchantOpCity.id.getId)
          merchantId = Just merchantOpCity.merchantId
          merchantOperatingCityId = merchantOpCity.id
          separator = "##XX##"

      ------------ TicketPlace --------------------------------------------------
      name <- cleanCSVField idx row.name "Name"
      allowSameDayBooking :: Bool <- readCSVField idx row.allowSameDayBooking "Allow same day booking"
      placeType :: PlaceType <- readCSVField idx row.placeType "Place Type"
      shortDesc <- cleanCSVField idx row.shortDesc "Short Description"
      gallery :: [Text] <- readCSVField idx row.gallery "Gallery"
      status :: PlaceStatus <- readCSVField idx row.status "Status"
      termsAndConditions :: [Text] <- readCSVField idx row.termsAndConditions "Terms and conditions"
      let description :: Maybe Text = cleanMaybeCSVField idx row.description "Description"
          openTimings :: Maybe TimeOfDay = readMaybeCSVField idx row.openTimings "Open timings"
          closeTimings :: Maybe TimeOfDay = readMaybeCSVField idx row.closeTimings "Close timings"
          iconUrl :: Maybe Text = cleanMaybeCSVField idx row.iconUrl "Icon URL"
          lat :: Maybe Double = readMaybeCSVField idx row.lat "Latitude"
          lon :: Maybe Double = readMaybeCSVField idx row.lon "Longitude"
          mapImageUrl :: Maybe Text = cleanMaybeCSVField idx row.mapImageUrl "Map Image URL"
          termsAndConditionsUrl :: Maybe Text = cleanMaybeCSVField idx row.termsAndConditionsUrl "Terms and conditions URL"
          ticketPlace = TicketPlace {id = ticketPlaceId, ..}

      ------------- TicketService --------------------------------------------------
      service <- cleanCSVField idx row.svc "Service"
      allowCancellation :: Bool <- readCSVField idx row.svcAllowCancellation "Service allow cancellation"
      allowFutureBooking :: Bool <- readCSVField idx row.svcAllowFutureBooking "Service allow future booking"
      maxVerification :: Int <- readCSVField idx row.svcMaxVerification "Service max verification"
      expiryType <- cleanCSVField idx row.svcExpiryType "Service expiry type"
      operationalDays :: [Text] <- readCSVField idx row.svcOperationalDays "Service operational days"
      let svcStartDate :: Maybe Day = readMaybeCSVField idx row.svcStartDate "Service start date"
          svcEndDate :: Maybe Day = readMaybeCSVField idx row.svcEndDate "Service end date"
      let operationalDate = case (svcStartDate, svcEndDate) of
            (Just startDt, Just endDt) -> Just OperationalDate {startDate = startDt, eneDate = endDt}
            _ -> Nothing
      expiry <- case expiryType of
        "InstantExpiry" -> do
          expiryValue :: Int <- readCSVField idx row.svcExpiryValue "Service expiry value"
          pure $ InstantExpiry expiryValue
        _ -> do
          expiryTime <- readCSVField idx row.svcExpiryTime "Service expiry time"
          pure $ VisitDate expiryTime
      let svcShortDesc :: Maybe Text = readMaybeCSVField idx row.shortDesc "Short Description"
          ticketServiceId = service <> separator <> ticketPlaceId.getId
          placesId = ticketPlaceId.getId
          ticketService =
            TicketService
              { id = Id ticketServiceId,
                businessHours = [],
                shortDesc = svcShortDesc,
                merchantOperatingCityId = Just merchantOperatingCityId,
                ..
              }

      ------------- Business Hour --------------------------------------------------
      bhType <- cleanCSVField idx row.businessHourType "Business hour type"
      (btype, bTypeId) <- case bhType of
        "Duration" -> do
          bhStartTime :: TimeOfDay <- readCSVField idx row.businessHourStartTime "Business hour start time"
          bhEndTime :: TimeOfDay <- readCSVField idx row.businessHourEndTime "Business hour end time"
          return (Duration bhStartTime bhEndTime, Id (show bhStartTime <> separator <> show bhEndTime <> separator <> ticketServiceId))
        _ -> do
          bhSlotTime :: TimeOfDay <- readCSVField idx row.businessHourSlotTime "Business hour slot time"
          return (Slot bhSlotTime, Id (show bhSlotTime <> separator <> ticketServiceId))
      let businessHour = BusinessHour {id = bTypeId, categoryId = [], merchantOperatingCityId = Just merchantOperatingCityId, ..}

      --------------- Service Category ------------------------------------------------
      svcCategoryDescription <- cleanCSVField idx row.svcCategoryDescription "Service Category Description"
      svcCategoryName <- cleanCSVField idx row.svcCategoryName "Service Category Name"
      let allowedSeats :: Maybe Int = readMaybeCSVField idx row.svcCategoryAllowedSeats "Allowed seats"
          availableSeats :: Maybe Int = readMaybeCSVField idx row.svcCategoryAvailableSeats "Available seats"
          svcCategoryId = svcCategoryName <> separator <> bTypeId.getId
      let serviceCategory =
            ServiceCategory
              { id = Id svcCategoryId,
                name = svcCategoryName,
                description = svcCategoryDescription,
                peopleCategory = [],
                merchantOperatingCityId = Just merchantOperatingCityId,
                ..
              }

      --------------- Service People Category ------------------------------------------------
      peopleCategoryDescription <- cleanCSVField idx row.peopleCategoryDescription "People Category Description"
      peopleCategoryName <- cleanCSVField idx row.peopleCategoryName "People Category Name"
      priceAmount :: HighPrecMoney <- readCSVField idx row.priceAmount "Price Amount"
      pricingType :: PricingType <- readCSVField idx row.pricingType "Pricing Type"
      priceCurrency :: Currency <- readCSVField idx row.priceCurrency "Price Currency"
      let vendorSplitDetails = (map Payment.roundVendorFee) <$> (cleanField row.vendorSplitDetails >>= JSON.decodeStrict . encodeUtf8)
          pricePerUnit = Price (round priceAmount) priceAmount priceCurrency
          mbPeakTimings = cleanField row.peakTimings
          svcPeopleCategoryId = peopleCategoryName <> separator <> svcCategoryId
          mbCancellationType = cleanField row.cancellationType
      timeBounds <-
        case mbPeakTimings of
          Nothing -> return Unbounded
          _ -> do
            peakTimings :: [(TimeOfDay, TimeOfDay)] <- readCSVField idx row.peakTimings "Peak Timings"
            peakDaysRaw :: [Text] <- readCSVField idx row.peakDays "Peak Days"
            let parsedDays = mapMaybe parseDayOrWeekday peakDaysRaw
                weekdays = [dow | Left dow <- parsedDays]
                specificDays = [day | Right day <- parsedDays]

            if null weekdays && null specificDays
              then return Unbounded
              else
                if null specificDays
                  then do
                    let bounds =
                          BoundedPeaks
                            { monday = if Monday `elem` weekdays then peakTimings else [],
                              tuesday = if Tuesday `elem` weekdays then peakTimings else [],
                              wednesday = if Wednesday `elem` weekdays then peakTimings else [],
                              thursday = if Thursday `elem` weekdays then peakTimings else [],
                              friday = if Friday `elem` weekdays then peakTimings else [],
                              saturday = if Saturday `elem` weekdays then peakTimings else [],
                              sunday = if Sunday `elem` weekdays then peakTimings else []
                            }
                    return $ BoundedByWeekday bounds
                  else do
                    let bounds = map (\d -> (d, peakTimings)) specificDays
                    return $ BoundedByDay bounds
      cancellationCharges <-
        case mbCancellationType of
          Nothing -> return Nothing
          _ -> do
            cancelationTypeTxt <- cleanCSVField idx row.cancellationType "Cancellation Type"
            let cancelationType =
                  T.strip cancelationTypeTxt
                    & T.dropAround (`elem` ['[', ']'])
                    & T.splitOn (T.pack ",")
                    & map T.strip
            cancellationTime :: [Seconds] <- readCSVField idx row.cancellationTime "Cancellation Time"
            cancellationFee :: [HighPrecMoney] <- readCSVField idx row.cancellationFee "Cancellation Fee"
            let len = length cancelationType
            if length cancellationTime == len && length cancellationFee == len
              then
                let maybeCharges = zipWith3 toCancellationCharge cancelationType cancellationTime cancellationFee
                 in if all isJust maybeCharges
                      then return $ Just (catMaybes maybeCharges)
                      else return Nothing
              else return Nothing
      let servicePeopleCategory =
            ServicePeopleCategory
              { id = Id svcPeopleCategoryId,
                name = peopleCategoryName,
                description = peopleCategoryDescription,
                merchantOperatingCityId = Just merchantOperatingCityId,
                ..
              }
      return (ticketPlace, ticketService, businessHour, serviceCategory, servicePeopleCategory)

    toCancellationCharge :: Text -> Seconds -> HighPrecMoney -> Maybe CancellationCharge
    toCancellationCharge type_ time fee =
      case T.toLower type_ of
        "flatfee" -> Just $ CancellationCharge (FlatFee fee) time
        "percentage" -> Just $ CancellationCharge (Percentage (round fee)) time
        _ -> Nothing

    parseDayOrWeekday :: Text -> Maybe (Either DayOfWeek Day)
    parseDayOrWeekday txt =
      case parseDayOfWeek txt of
        Just dow -> Just (Left dow)
        Nothing -> Right <$> parseDate txt

    parseDayOfWeek :: Text -> Maybe DayOfWeek
    parseDayOfWeek "Monday" = Just Monday
    parseDayOfWeek "Tuesday" = Just Tuesday
    parseDayOfWeek "Wednesday" = Just Wednesday
    parseDayOfWeek "Thursday" = Just Thursday
    parseDayOfWeek "Friday" = Just Friday
    parseDayOfWeek "Saturday" = Just Saturday
    parseDayOfWeek "Sunday" = Just Sunday
    parseDayOfWeek _ = Nothing

    parseDate :: Text -> Maybe Day
    parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack
