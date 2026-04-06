{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.Sos where

import qualified API.Types.RiderPlatform.Management.Sos
import qualified API.Types.UI.Sos as UISos
import qualified Dashboard.Common
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Action.UI.Profile as DP
import qualified Domain.Action.UI.Sos as Sos
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as DPerson
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderConfig as DRC
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.External.SOS as PoliceSOS
import qualified Kernel.External.SOS.ERSS.Auth as ERSSAuth
import qualified Kernel.External.SOS.Interface.Types as SOSInterface
import qualified Kernel.External.SOS.Types as SOS
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import qualified Safety.Domain.Types.Common as SafetyDCommon
import qualified Safety.Domain.Types.Sos as SafetyDSos
import qualified Safety.Storage.Queries.Sos as SafetyQSos
import Servant hiding (throwError)
import Servant.Client (showBaseUrl)
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import qualified SharedLogic.External.LocationTrackingService.Types as LTSTypes
import qualified SharedLogic.Ride as SRide
import qualified SharedLogic.SosLocationTracking as SOSLocation
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Getter (getConfig)
import Storage.ConfigPilot.Interface.Types (getOneConfig)
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import Tools.Auth
import Tools.Error
import qualified Tools.Notifications as Notify

getSosTracking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.Flow API.Types.RiderPlatform.Management.Sos.SosTrackingRes)
getSosTracking _merchantShortId _opCity sosId = do
  sosTrackingRateLimitOptions <- asks (.sosTrackingRateLimitOptions)
  checkSlidingWindowLimitWithOptions (sosDashboardHitsCountKey sosId) sosTrackingRateLimitOptions
  let sosId' = Kernel.Types.Id.cast @Dashboard.Common.Sos @SafetyDSos.Sos sosId
  res <- Sos.getSosTracking sosId'
  pure $ convertToApiRes res

getSosDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> Environment.Flow API.Types.RiderPlatform.Management.Sos.SosDetailsMaybeRes)
getSosDetails merchantShortId opCity sosId = do
  sosTrackingRateLimitOptions <- asks (.sosTrackingRateLimitOptions)
  checkSlidingWindowLimitWithOptions (sosDashboardHitsCountKey sosId) sosTrackingRateLimitOptions
  let sosId' = Kernel.Types.Id.cast @Dashboard.Common.Sos @SafetyDSos.Sos sosId
  sos <- B.runInReplica $ SafetyQSos.findById sosId' >>= fromMaybeM (InvalidRequest $ "SOS not found: " <> sosId'.getId)
  mbMerchantOpCity <- case sos.merchantOperatingCityId of
    Just cityId -> CQMOC.findById (Kernel.Types.Id.cast cityId)
    Nothing -> CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
  mbRideConfig <- maybe (pure Nothing) (\moc -> getConfig (RiderDimensions {merchantOperatingCityId = moc.id.getId})) mbMerchantOpCity
  let externalSOSConfig = mbRideConfig >>= \rc -> rc.externalSOSConfig
  let triggerSource = convertTriggerSource <$> (externalSOSConfig <&> (.triggerSource))
  person <- B.runInReplica $ QP.findById (Kernel.Types.Id.cast @SafetyDCommon.Person @DPerson.Person sos.personId) >>= fromMaybeM (PersonNotFound sos.personId.getId)
  let riderDetails =
        API.Types.RiderPlatform.Management.Sos.RiderDetailsRes
          { firstName = person.firstName,
            middleName = person.middleName,
            lastName = person.lastName
          }
  let mbSosTrackingUrl = do
        rideConfig <- mbRideConfig
        pure $ Notify.buildSosTrackingUrl sos.id rideConfig.trackingShortUrlPattern
  pure $
    API.Types.RiderPlatform.Management.Sos.SosDetailsMaybeRes
      { API.Types.RiderPlatform.Management.Sos.details = Just $ convertToSosDetailsRes sos,
        API.Types.RiderPlatform.Management.Sos.triggerSource = triggerSource,
        API.Types.RiderPlatform.Management.Sos.riderDetails = Just riderDetails,
        API.Types.RiderPlatform.Management.Sos.trackingUrl = mbSosTrackingUrl
      }

convertToSosDetailsRes :: SafetyDSos.Sos -> API.Types.RiderPlatform.Management.Sos.SosDetailsRes
convertToSosDetailsRes s =
  API.Types.RiderPlatform.Management.Sos.SosDetailsRes
    { id = Kernel.Types.Id.cast @SafetyDSos.Sos @Dashboard.Common.Sos s.id,
      personId = Kernel.Types.Id.cast @SafetyDCommon.Person @Dashboard.Common.Customer s.personId,
      rideId = Kernel.Types.Id.cast @SafetyDCommon.Ride @Dashboard.Common.Ride <$> s.rideId,
      flow = convertSosType s.flow,
      status = convertSosStatus s.status,
      ticketId = s.ticketId,
      mediaFiles = Kernel.Types.Id.getId <$> s.mediaFiles,
      trackingExpiresAt = s.trackingExpiresAt,
      sosState = convertSosState <$> s.sosState,
      entityType = convertSosEntityType <$> s.entityType,
      externalReferenceId = s.externalReferenceId,
      externalReferenceStatus = s.externalReferenceStatus,
      externalStatusHistory = s.externalStatusHistory,
      merchantId = Kernel.Types.Id.cast @SafetyDCommon.Merchant @Dashboard.Common.Merchant <$> s.merchantId,
      merchantOperatingCityId = Kernel.Types.Id.cast @SafetyDCommon.MerchantOperatingCity @Dashboard.Common.MerchantOperatingCity <$> s.merchantOperatingCityId,
      createdAt = s.createdAt,
      updatedAt = s.updatedAt
    }

convertSosType :: SafetyDSos.SosType -> API.Types.RiderPlatform.Management.Sos.SosType
convertSosType SafetyDSos.Police = API.Types.RiderPlatform.Management.Sos.Police
convertSosType SafetyDSos.CustomerCare = API.Types.RiderPlatform.Management.Sos.CustomerCare
convertSosType (SafetyDSos.EmergencyContact _) = API.Types.RiderPlatform.Management.Sos.SafetyFlow
convertSosType SafetyDSos.SafetyFlow = API.Types.RiderPlatform.Management.Sos.SafetyFlow
convertSosType SafetyDSos.CSAlertSosTicket = API.Types.RiderPlatform.Management.Sos.CSAlertSosTicket
convertSosType SafetyDSos.AudioRecording = API.Types.RiderPlatform.Management.Sos.AudioRecording
convertSosType SafetyDSos.KaptureDashboard = API.Types.RiderPlatform.Management.Sos.KaptureDashboard

convertSosEntityType :: SafetyDSos.SosEntityType -> API.Types.RiderPlatform.Management.Sos.SosEntityType
convertSosEntityType SafetyDSos.Ride = API.Types.RiderPlatform.Management.Sos.Ride
convertSosEntityType SafetyDSos.NonRide = API.Types.RiderPlatform.Management.Sos.NonRide

convertTriggerSource :: DRC.ExternalSOSTriggerSource -> API.Types.RiderPlatform.Management.Sos.ExternalSOSTriggerSource
convertTriggerSource DRC.FRONTEND = API.Types.RiderPlatform.Management.Sos.FRONTEND
convertTriggerSource DRC.DASHBOARD = API.Types.RiderPlatform.Management.Sos.DASHBOARD

-- Convert from rider-app SosTrackingRes to dashboard API SosTrackingRes
convertToApiRes :: UISos.SosTrackingRes -> API.Types.RiderPlatform.Management.Sos.SosTrackingRes
convertToApiRes r =
  API.Types.RiderPlatform.Management.Sos.SosTrackingRes
    { currentLocation = convertLocation <$> r.currentLocation,
      sosState = convertSosState <$> r.sosState,
      status = convertSosStatus r.status
    }

convertLocation :: UISos.SosLocationRes -> API.Types.RiderPlatform.Management.Sos.SosLocationRes
convertLocation loc =
  API.Types.RiderPlatform.Management.Sos.SosLocationRes
    { lat = loc.lat,
      lon = loc.lon,
      accuracy = loc.accuracy
    }

convertSosState :: SafetyDSos.SosState -> API.Types.RiderPlatform.Management.Sos.SosState
convertSosState SafetyDSos.LiveTracking = API.Types.RiderPlatform.Management.Sos.LiveTracking
convertSosState SafetyDSos.SosActive = API.Types.RiderPlatform.Management.Sos.SosActive

convertSosStatus :: SafetyDSos.SosStatus -> API.Types.RiderPlatform.Management.Sos.SosStatus
convertSosStatus SafetyDSos.Resolved = API.Types.RiderPlatform.Management.Sos.Resolved
convertSosStatus SafetyDSos.NotResolved = API.Types.RiderPlatform.Management.Sos.NotResolved
convertSosStatus SafetyDSos.Pending = API.Types.RiderPlatform.Management.Sos.Pending
convertSosStatus SafetyDSos.MockPending = API.Types.RiderPlatform.Management.Sos.MockPending
convertSosStatus SafetyDSos.MockResolved = API.Types.RiderPlatform.Management.Sos.MockResolved

-- | Called from the dashboard when triggerSource is DASHBOARD.
--   Fetches the SOS record and dispatches the external SOS API call.
callExternalSOS :: Kernel.Types.Id.Id SafetyDSos.Sos -> Maybe Text -> Environment.Flow ()
callExternalSOS sosId mbComments = do
  sos <- SafetyQSos.findById sosId >>= fromMaybeM (InvalidRequest "SOS record not found")
  merchantOpCityId <- Kernel.Types.Id.cast @SafetyDCommon.MerchantOperatingCity @DMOC.MerchantOperatingCity <$> sos.merchantOperatingCityId & fromMaybeM (InvalidRequest "SOS record missing merchantOperatingCityId")
  merchantId <- Kernel.Types.Id.cast @SafetyDCommon.Merchant @Domain.Types.Merchant.Merchant <$> sos.merchantId & fromMaybeM (InvalidRequest "SOS record missing merchantId")
  person <- QP.findById (Kernel.Types.Id.cast @SafetyDCommon.Person @DPerson.Person sos.personId) >>= fromMaybeM (PersonDoesNotExist sos.personId.getId)
  riderConfig <- getConfig (RiderDimensions merchantOpCityId.getId) >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
  case riderConfig.externalSOSConfig of
    Nothing -> throwError $ InvalidRequest "External SOS config not configured for this city"
    Just sosConfig -> do
      when (sosConfig.triggerSource /= DRC.DASHBOARD) $
        throwError $ InvalidRequest "External SOS trigger source is not DASHBOARD for this city"
      let sosServiceType = flowToSOSService sosConfig.flow
      merchantSvcCfg <-
        getOneConfig (MerchantServiceConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, merchantId = merchantId.getId, serviceName = Just (DMSC.SOSService sosServiceType)})
          >>= fromMaybeM (MerchantServiceConfigNotFound merchantOpCityId.getId "SOS" (show sosServiceType))
      case merchantSvcCfg.serviceConfig of
        DMSC.SOSServiceConfig specificConfig -> do
          mbRide <- maybe (pure Nothing) (\rid -> QRide.findById (Kernel.Types.Id.cast @SafetyDCommon.Ride @DRide.Ride rid)) sos.rideId
          let rideIdForLoc = Kernel.Types.Id.cast @SafetyDCommon.Ride @DRide.Ride <$> sos.rideId
          customerLocation <- getCustomerLocation rideIdForLoc mbRide
          emergencyContacts <- DP.getDefaultEmergencyNumbers (Kernel.Types.Id.cast @SafetyDCommon.Person @DPerson.Person sos.personId, merchantId)
          merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
          externalSOSDetails <- Sos.buildExternalSOSDetails (mkSosReq sos customerLocation) person sosConfig specificConfig mbRide emergencyContacts.defaultEmergencyNumbers merchantOpCity riderConfig mbComments
          initialRes <- PoliceSOS.sendInitialSOS specificConfig externalSOSDetails
          unless initialRes.success $
            throwError $ InternalError (fromMaybe "External SOS call failed" initialRes.errorMessage)
          whenJust initialRes.trackingId $ \trackingId -> do
            SafetyQSos.updateExternalReferenceId (Just trackingId) sosId
            Redis.del (Sos.mkExternalSOSTraceKey sosId)
            -- Register ERSS config with LTS for location trace forwarding
            case specificConfig of
              SOSInterface.ERSSConfig erssCfg -> do
                mbMobile <- Sos.getPersonMobileNo (Kernel.Types.Id.cast @SafetyDCommon.Person @DPerson.Person sos.personId)
                whenJust mbMobile $ \mobileNo -> do
                  erssToken <- ERSSAuth.getERSSToken erssCfg
                  selfBaseUrl <- asks (.selfBaseUrl)
                  locationTrackingServiceKey <- asks (.locationTrackingServiceKey)
                  let nyReauthUrl = T.pack (showBaseUrl selfBaseUrl) <> "/internal/sos/erss-reauth"
                      tokenExpiresAt = round (utcTimeToPOSIXSeconds erssToken.accessExpiresAt) :: Int
                  let erssReq =
                        LTSTypes.SosErssConfigReq
                          { externalReferenceId = trackingId,
                            baseUrl = T.pack (showBaseUrl erssCfg.baseUrl),
                            accessToken = erssToken.accessToken,
                            tokenExpiresAt = tokenExpiresAt,
                            nyReauthUrl = nyReauthUrl,
                            nyApiKey = locationTrackingServiceKey,
                            merchantOperatingCityId = merchantOpCityId.getId,
                            pollingIntervalSecs = maybe 60 (.getSeconds) (riderConfig.externalSOSConfig >>= (.tracePollingIntervalSeconds)),
                            timeDiffSecs = riderConfig.timeDiffFromUtc.getSeconds,
                            expiresAt = Nothing,
                            provider =
                              LTSTypes.SosTraceProvider
                                { providerKind = LTSTypes.ERSS,
                                  authId = erssCfg.authId,
                                  authCode = erssCfg.authCode,
                                  mobileNo = mobileNo,
                                  traceUrlSuffix = "/public/api/sos/trace"
                                }
                          }
                  fork "ltsSosEntityUpsert" $
                    LTS.entityUpsertForSos sosId.getId (sos.personId.getId) merchantId.getId (Just erssReq)
              _ -> pure ()
        _ -> throwError $ InternalError "Invalid SOS Service Config for provider"
  where
    getCustomerLocation :: Maybe (Kernel.Types.Id.Id DRide.Ride) -> Maybe DRide.Ride -> Environment.Flow (Maybe LatLong)
    getCustomerLocation rideId mbRide = do
      mbSosLoc <- SOSLocation.getSosRiderLocation sosId
      case mbSosLoc of
        Just sosLoc -> do
          logInfo $ "Using SOS rider location from Redis for sosId: " <> sosId.getId
          pure $ Just $ LatLong sosLoc.lat sosLoc.lon
        Nothing -> do
          logInfo $ "SOS rider location not found, trying driver location for sosId: " <> sosId.getId
          case rideId of
            Just rid -> do
              driverLocResp <- withTryCatch "getDriverLoc:callExternalSOS" $ SRide.getDriverLoc rid
              case driverLocResp of
                Right driverLoc -> pure $ Just $ LatLong driverLoc.lat driverLoc.lon
                Left err -> do
                  logError $ "Driver location fetch failed, falling back to ride fromLocation: " <> show err
                  pure $ (\ride -> LatLong ride.fromLocation.lat ride.fromLocation.lon) <$> mbRide
            Nothing -> pure $ (\ride -> LatLong ride.fromLocation.lat ride.fromLocation.lon) <$> mbRide

    mkSosReq :: SafetyDSos.Sos -> Maybe LatLong -> UISos.SosReq
    mkSosReq sos customerLoc =
      UISos.SosReq
        { flow = sos.flow,
          rideId = Kernel.Types.Id.cast @SafetyDCommon.Ride @DRide.Ride <$> sos.rideId,
          isRideEnded = Nothing,
          sendPNOnPostRideSOS = Nothing,
          notifyAllContacts = Nothing,
          customerLocation = customerLoc,
          triggerApiList = Nothing
        }

flowToSOSService :: DRC.ExternalSOSFlow -> SOS.SOSService
flowToSOSService DRC.ERSS = SOS.ERSS
flowToSOSService DRC.GJ112 = SOS.GJ112
flowToSOSService DRC.Trinity = SOS.Trinity

postSosCallExternalSOS :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Sos -> API.Types.RiderPlatform.Management.Sos.CallExternalSOSReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postSosCallExternalSOS _merchantShortId _opCity sosId req = do
  let sosId' = Kernel.Types.Id.cast @Dashboard.Common.Sos @SafetyDSos.Sos sosId
  callExternalSOS sosId' req.comments
  pure Kernel.Types.APISuccess.Success

sosDashboardHitsCountKey :: Kernel.Types.Id.Id Dashboard.Common.Sos -> Text
sosDashboardHitsCountKey sosId = "SosDashboardHits:hitsCount:" <> Kernel.Types.Id.getId sosId

postSosErssStatusUpdate :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.Sos.ErssStatusUpdateReq -> Environment.Flow API.Types.RiderPlatform.Management.Sos.ErssStatusUpdateRes
postSosErssStatusUpdate _merchantShortId _opCity req = do
  sosDetails <-
    SafetyQSos.findByExternalReferenceId (Just req.idErss)
      >>= fromMaybeM (InvalidRequest $ "No SOS found for signal ID: " <> req.idErss)

  let newStatus = req.currentStatus
      newEntry = Sos.ExternalStatusEntry {status = newStatus, idErss = req.idErss, lastUpdatedTime = req.lastUpdatedTime}
      existingHistory = maybe [] (\h -> fromMaybe [] (A.decode (LBS.fromStrict $ TE.encodeUtf8 h))) sosDetails.externalStatusHistory :: [Sos.ExternalStatusEntry]
      updatedHistory = existingHistory <> [newEntry]
      updatedHistoryJson = Just $ TE.decodeUtf8 $ LBS.toStrict $ A.encode updatedHistory

  SafetyQSos.updateExternalReferenceStatus (Just newStatus) updatedHistoryJson sosDetails.id

  when (newStatus == "RESOLVED") $ do
    SafetyQSos.updateStatus SafetyDSos.Resolved sosDetails.id

  pure $
    API.Types.RiderPlatform.Management.Sos.ErssStatusUpdateRes
      { resultCode = "OPERATION_SUCCESS",
        resultString = Just "Status update received successfully",
        errorMsg = Nothing,
        message = Nothing,
        payLoad = Nothing
      }
