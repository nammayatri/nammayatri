{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.Sos where

-- Domain.Types.Sos removed - using Safety.Domain.Types.Sos everywhere

-- Keep for mockSosKey only

import Control.Applicative
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import Data.Text as T hiding (map)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format
import qualified Domain.Action.UI.Call as DUCall
import qualified Domain.Action.UI.FollowRide as DFR
import Domain.Action.UI.Profile ()
-- HasEmergencyContactHandle instance

import qualified Domain.Types.CallStatus as DCall
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderConfig as DRC
import Environment
import qualified IssueManagement.Common as IC
import Kernel.Beam.Functions
import Kernel.External.Encryption
import qualified Kernel.External.IncidentReport as IncidentReport
import Kernel.External.IncidentReport.Interface.Types as IncidentReportTypes
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.External.Notification as Notification
import qualified Kernel.External.SOS as PoliceSOS
import qualified Kernel.External.SOS.ERSS.Auth as ERSSAuth
import qualified Kernel.External.SOS.ERSS.Config as ERSSConfig
import qualified Kernel.External.SOS.Interface.Types as SOSInterface
import qualified Kernel.External.SOS.Types as SOS
import Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import qualified Safety.API.Types.UI.PersonDefaultEmergencyNumber as EmergencyAPI
import Safety.API.Types.UI.Sos
import qualified Safety.Domain.Action.UI.PersonDefaultEmergencyNumber as EmergencyLib
import qualified Safety.Domain.Action.UI.Sos as SafetySos
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.Sos as SafetyDSos
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import qualified SharedLogic.External.LocationTrackingService.Types as LTSTypes
import SharedLogic.JobScheduler
import qualified SharedLogic.MessageBuilder as MessageBuilder
import SharedLogic.Person as SLP
import SharedLogic.PersonDefaultEmergencyNumber as SPDEN
import qualified SharedLogic.Ride as SRide
import SharedLogic.Scheduler.Jobs.CallPoliceApi
import SharedLogic.Scheduler.Jobs.SafetyCSAlert as SIVR
import Storage.Beam.IssueManagement ()
import Storage.Beam.SchedulerJob ()
import Storage.Beam.Sos ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.MerchantServiceConfig (MerchantServiceConfigDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.PersonDisability as PDisability
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Call as Call
import Tools.Error
import Tools.Ticket as Ticket

-- | Convert rider-local DRC.ExternalSOSConfig to the shared SafetyCommon.ExternalSOSConfig
-- that the unified API types use. Single boundary conversion — applied at every
-- read of @riderConfig.externalSOSConfig@ so the rest of the rider code (and
-- shared-services) speaks SafetyCommon types end-to-end.
toSafetyExternalSOSConfig :: DRC.ExternalSOSConfig -> SafetyCommon.ExternalSOSConfig
toSafetyExternalSOSConfig cfg =
  SafetyCommon.ExternalSOSConfig
    { flow = case cfg.flow of
        DRC.ERSS -> SafetyCommon.ERSS
        DRC.GJ112 -> SafetyCommon.GJ112
        DRC.Trinity -> SafetyCommon.Trinity,
      latLonRequired = cfg.latLonRequired,
      mediaRequired = cfg.mediaRequired,
      stateCode = cfg.stateCode,
      tracePollingIntervalSeconds = cfg.tracePollingIntervalSeconds,
      trackingLinkRequired = cfg.trackingLinkRequired,
      triggerSource = case cfg.triggerSource of
        DRC.FRONTEND -> SafetyCommon.FRONTEND
        DRC.DASHBOARD -> SafetyCommon.DASHBOARD
    }
incidentReportHandler :: Person.Person -> Id Merchant.Merchant -> Id DMOC.MerchantOperatingCity -> Id DRide.Ride -> Text -> Maps.LatLong -> DRC.RiderConfig -> Flow ()
incidentReportHandler person merchantId merchantOpCityId rideId token coordinates riderConfig = do
  logDebug "Initiating Incident Report Police Call"
  merchantIRSvcCfg <- getServiceConfig merchantId merchantOpCityId (DMSC.IncidentReportService ERSS)
  contactNo <- fmap (fromMaybe "0000000000") $ traverse decrypt person.mobileNumber
  res <-
    IncidentReport.reportIncident
      merchantIRSvcCfg
      IncidentReportTypes.IncidentReportReq
        { latitude = coordinates.lat,
          longitude = coordinates.lon,
          mpin = "",
          contactNo = contactNo,
          token = token
        }
  logDebug $ "Incident Report API Response: " <> show res
  scheduleIncidentFollowUp res
  where
    scheduleIncidentFollowUp :: IncidentReportRes -> Flow ()
    scheduleIncidentFollowUp res = do
      let scheduleAfter = max 2 riderConfig.policeTriggerDelay
          callPoliceAPIJobData = CallPoliceApiJobData {rideId, personId = person.id, jmCode = res.incidentData.jmCode}
      logDebug $ "Scheduling safety alert police call after : " <> show scheduleAfter
      Redis.withCrossAppRedis $ Redis.setExp (mkRideCallPoliceAPIKey rideId) (1 :: Int) riderConfig.hardLimitForSafetyJobs
      createJobIn @_ @'CallPoliceApi (Just merchantId) (Just merchantOpCityId) scheduleAfter callPoliceAPIJobData
      pure ()

sendUnattendedSosTicketAlert :: Text -> Flow ()
sendUnattendedSosTicketAlert ticketId = do
  unattendedSosRedisKey :: Maybe Bool <- Redis.safeGet mkUnattendedSosAlertKey
  case unattendedSosRedisKey of
    Just _value -> do
      logTagInfo "Unattended SOS Ticket Alert" ("Alert discarded for Ticket ID: " <> ticketId)
      return ()
    Nothing -> do
      Redis.setExp mkUnattendedSosAlertKey True 60
      sos <- SafetySos.findSosByTicketId (Just ticketId) >>= fromMaybeM (InvalidRequest $ "SOS with ticketId-" <> ticketId <> " does not exist.")
      merchantOpCityId <-
        maybe
          ( ( QP.findById (cast sos.personId)
                >>= fromMaybeM (PersonNotFound (getId sos.personId))
            )
              <&> (.merchantOperatingCityId)
          )
          (return . cast)
          sos.merchantOperatingCityId
      merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
      riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOpCityId.getId}) >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
      let maybeAppId = (HM.lookup DRC.UnattendedTicketAppletID . DRC.exotelMap) =<< riderConfig.exotelAppIdMapping
      mapM_ (sendAlert merchantOperatingCity sos maybeAppId) (fromMaybe [] riderConfig.cxAgentDetails)
  where
    sendAlert :: DMOC.MerchantOperatingCity -> SafetyDSos.Sos -> Maybe Text -> IC.CxAgentDetails -> Flow ()
    sendAlert merchantOpCity sos maybeAppId cxAgentDetails =
      when (SafetySos.isRideBasedSos sos.entityType) $ do
        fork ("Sending unattended sos ticket alert to agentDetails - " <> show cxAgentDetails) $ do
          callStatusId <- generateGUID
          rideId <- sos.rideId & fromMaybeM (RideDoesNotExist "Ride ID not found")
          let callReq =
                Call.InitiateCallReq
                  { fromPhoneNum = cxAgentDetails.agentMobileNumber,
                    toPhoneNum = Nothing,
                    attachments = Call.Attachments $ DUCall.CallAttachments {callStatusId = callStatusId, rideId = cast @SafetyCommon.Ride @DRide.Ride rideId},
                    appletId = maybeAppId
                  }
          exotelResponse <- Call.initiateCall merchantOpCity.merchantId merchantOpCity.id callReq
          callStatus <- buildCallStatus callStatusId exotelResponse sos merchantOpCity.id
          QCallStatus.create callStatus

    buildCallStatus :: Id DCall.CallStatus -> Call.InitiateCallResp -> SafetyDSos.Sos -> Id DMOC.MerchantOperatingCity -> Flow DCall.CallStatus
    buildCallStatus callStatusId exotelResponse sos merchantOpCityId = do
      now <- getCurrentTime
      return $
        DCall.CallStatus
          { id = callStatusId,
            callId = exotelResponse.callId,
            rideId = cast <$> sos.rideId,
            dtmfNumberUsed = Nothing,
            status = exotelResponse.callStatus,
            callAttempt = Nothing,
            conversationDuration = 0,
            recordingUrl = Nothing,
            merchantId = (cast <$> sos.merchantId) <&> (.getId),
            merchantOperatingCityId = Just merchantOpCityId,
            callService = Just Call.Exotel,
            callError = Nothing,
            createdAt = now,
            updatedAt = now,
            customerIvrResponse = Nothing
          }

    mkUnattendedSosAlertKey :: Text
    mkUnattendedSosAlertKey = "Unattended:SOS:Alert:TicketId-" <> ticketId
-- | Wires rider's app-specific config readers into the shared trace-polling
-- skeleton. Cache key, dispatch decision, and trace-API call all live in
-- 'SafetySos.runExternalSosTrace'.
sendExternalSOSTrace :: SafetyDSos.Sos -> SosLocationUpdateReq -> Flow ()
sendExternalSOSTrace =
  SafetySos.runExternalSosTrace
    (\mbMocId -> getExternalSOSTracePollingConfig (cast <$> mbMocId))
    (\sharedPersonId -> getPersonMobileNo (cast sharedPersonId))
    resolveExternalSOSTraceConfig

getExternalSOSTracePollingConfig :: Maybe (Id DMOC.MerchantOperatingCity) -> Flow (Int, Seconds)
getExternalSOSTracePollingConfig = \case
  Nothing -> pure (defaultExternalSOSTracePollingIntervalSec, defaultTimeDiff)
  Just merchantOpCityId -> do
    mbRiderConfig <- getConfig (RiderDimensions merchantOpCityId.getId)
    case mbRiderConfig of
      Nothing -> pure (defaultExternalSOSTracePollingIntervalSec, defaultTimeDiff)
      Just rc ->
        let interval = maybe defaultExternalSOSTracePollingIntervalSec (.getSeconds) (rc.externalSOSConfig >>= (.tracePollingIntervalSeconds))
         in pure (interval, rc.timeDiffFromUtc)
  where
    defaultExternalSOSTracePollingIntervalSec = 60
    defaultTimeDiff = 19800 -- IST: 5h 30m

resolveExternalSOSTraceConfig :: SafetyDSos.Sos -> Flow (Maybe SOSInterface.SOSServiceConfig)
resolveExternalSOSTraceConfig sosDetails = do
  case (sosDetails.externalReferenceId, sosDetails.merchantId, sosDetails.merchantOperatingCityId) of
    (Just _, Just merchantId, Just merchantOpCityId) -> do
      mbRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOpCityId.getId})
      case toSafetyExternalSOSConfig <$> (mbRiderConfig >>= (.externalSOSConfig)) of
        Just sosConfig | sosConfig.latLonRequired ->
          getSosSpecificConfig (cast merchantId) (cast merchantOpCityId) (SafetyCommon.flowToSOSService sosConfig.flow)
        _ -> pure Nothing
    _ -> pure Nothing

getPersonMobileNo :: Id Person.Person -> Flow (Maybe Text)
getPersonMobileNo personId =
  runInReplica $
    QP.findById personId >>= \mbPerson ->
      maybe (pure Nothing) (\p -> mapM decrypt p.mobileNumber) mbPerson

-- | Look up the resolved external-SOS service config (police/ERSS/etc.) for a
-- merchant + city + flow combination. Returns 'Nothing' when the merchant
-- service config is absent or stored as a non-SOS variant. Used at every
-- external-SOS entry point so callers don't repeat the @getOneConfig@ +
-- @SOSServiceConfig@ unwrap.
getSosSpecificConfig ::
  Id Merchant.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  SOS.SOSService ->
  Flow (Maybe SOSInterface.SOSServiceConfig)
getSosSpecificConfig mId mocId sosServiceType = do
  mbMerchantSvcCfg <-
    getOneConfig
      ( MerchantServiceConfigDimensions
          { merchantOperatingCityId = mocId.getId,
            merchantId = mId.getId,
            serviceName = Just (DMSC.SOSService sosServiceType)
          }
      )
  pure $ case mbMerchantSvcCfg of
    Just cfg -> case cfg.serviceConfig of
      DMSC.SOSServiceConfig specificConfig -> Just specificConfig
      _ -> Nothing
    Nothing -> Nothing

-- | Build the LTS broadcaster config (ERSS-only). Centralizes the ERSS token
-- fetch + AppEnv lookups (selfBaseUrl, locationTrackingServiceKey) so the
-- trigger handler ('riderRegisterSosWithLts') and the dashboard re-trigger
-- ('Dashboard.Sos.callExternalSOS') don't reproduce the same 25-line record
-- assembly.
buildSosErssConfigReq ::
  Text ->
  ERSSConfig.ERSSCfg ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Seconds ->
  Seconds ->
  Text ->
  Flow LTSTypes.SosErssConfigReq
buildSosErssConfigReq refId erssCfg mocId mbPollingInterval timeDiffFromUtc mobileNo = do
  erssToken <- ERSSAuth.getERSSToken erssCfg
  selfBaseUrl <- asks (.selfBaseUrl)
  locationTrackingServiceKey <- asks (.locationTrackingServiceKey)
  let nyReauthUrl = showBaseUrl selfBaseUrl <> "/internal/sos/erss-reauth"
      tokenExpiresAt = round (utcTimeToPOSIXSeconds erssToken.accessExpiresAt) :: Int
      pollingInterval = maybe 60 (.getSeconds) mbPollingInterval
  pure
    LTSTypes.SosErssConfigReq
      { externalReferenceId = refId,
        baseUrl = showBaseUrl erssCfg.baseUrl,
        accessToken = erssToken.accessToken,
        tokenExpiresAt = tokenExpiresAt,
        nyReauthUrl = nyReauthUrl,
        nyApiKey = locationTrackingServiceKey,
        merchantOperatingCityId = mocId.getId,
        pollingIntervalSecs = pollingInterval,
        timeDiffSecs = timeDiffFromUtc.getSeconds,
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

handleExternalSOS :: Person.Person -> SafetySos.SosPersonData -> SafetyCommon.ExternalSOSConfig -> SosReq -> Flow (Maybe Text, Maybe SOSInterface.SOSServiceConfig)
handleExternalSOS person personData sosConfig req = do
  let sosServiceType = SafetyCommon.flowToSOSService sosConfig.flow
      appMerchantOpCityId = person.merchantOperatingCityId
      appMerchantId = person.merchantId
      appPersonId = person.id
  mbSpecificConfig <- getSosSpecificConfig appMerchantId appMerchantOpCityId sosServiceType
  case mbSpecificConfig of
    Nothing -> do
      logError $ "handleExternalSOS: SOS service config not configured for service " <> show sosServiceType
      return (Nothing, Nothing)
    Just specificConfig -> do
      mbRide <- maybe (pure Nothing) (\rideId -> QRide.findById (cast @SafetyCommon.Ride @DRide.Ride rideId)) req.rideId
      emergencyContacts <- EmergencyLib.getEmergencyContacts (appPersonId, appMerchantId)
      merchantOpCity <- CQMOC.findById appMerchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound appMerchantOpCityId.getId)
      externalSOSDetails <- buildExternalSOSDetails req person personData sosConfig specificConfig mbRide emergencyContacts.defaultEmergencyNumbers merchantOpCity Nothing Nothing
      initialRes <- PoliceSOS.sendInitialSOS specificConfig externalSOSDetails
      if initialRes.success
        then return (initialRes.trackingId, Just specificConfig)
        else do
          logError $ "handleExternalSOS: External SOS call failed: " <> fromMaybe "Unknown error" initialRes.errorMessage
          return (Nothing, Nothing)

buildExternalSOSDetails ::
  SosReq ->
  Person.Person ->
  SafetySos.SosPersonData ->
  SafetyCommon.ExternalSOSConfig ->
  SOSInterface.SOSServiceConfig ->
  Maybe DRide.Ride ->
  [EmergencyAPI.EmergencyContact] ->
  DMOC.MerchantOperatingCity ->
  Maybe Text ->
  Maybe (Id SafetyDSos.Sos) ->
  Flow SOSInterface.InitialSOSReq
buildExternalSOSDetails req person personData sosConfig _serviceConfig mbRide emergencyContacts merchantOpCity mbComments mbSosId = do
  now <- getCurrentTime
  mobileNo <- fmap (fromMaybe "0000000000") $ traverse decrypt person.mobileNumber
  emailText <- traverse decrypt person.email
  imeiText <- traverse decrypt person.imeiNumber
  customerDisability <- runInReplica $ PDisability.findByPersonId person.id
  (lat, lon) <- resolveLocation req.customerLocation person mbRide
  let localNow = addUTCTime (secondsToNominalDiffTime personData.timeDiffFromUtc) now
  let dateTimeStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localNow
  let trackLink =
        mbSosId <&> \sosId ->
          case personData.trackingUrlPattern of
            Just sosLink -> T.replace "{#vp#}" "sosTracking" sosLink <> sosId.getId
            Nothing ->
              T.replace "{#vp#}" "shareRide" personData.shareRideTrackingUrlPattern
                <> fromMaybe "" (mbRide <&> (.id.getId))
  let (ec1Name, ec1Phone, ec2Name, ec2Phone) = case emergencyContacts of
        (c1 : c2 : _) -> (Just c1.name, Just c1.mobileNumber, Just c2.name, Just c2.mobileNumber)
        (c1 : _) -> (Just c1.name, Just c1.mobileNumber, Nothing, Nothing)
        _ -> (Nothing, Nothing, Nothing, Nothing)
  let dobStr = T.pack . formatTime defaultTimeLocale "%Y-%m-%d" <$> person.dateOfBirth
  let specialNeedsText =
        customerDisability <&> \d ->
          case d.description of
            Just desc -> d.tag <> " - " <> desc
            Nothing -> d.tag
  return $
    SOSInterface.InitialSOSReq
      { sosId = Nothing,
        dateTime = dateTimeStr,
        latitude = lat,
        longitude = lon,
        speed = Nothing,
        mobileNo = mobileNo,
        imeiNo = imeiText,
        gpsProvider = Nothing,
        senderName = person.firstName,
        address = Nothing,
        gpsAccuracy = Nothing,
        stateCode = sosConfig.stateCode,
        silentCommunication = Nothing,
        specialNeeds = mbComments <|> specialNeedsText,
        dob = dobStr,
        gender = Just $ T.pack $ show person.gender,
        attachmentFileName = Nothing,
        driverName = (.driverName) <$> mbRide,
        driverContactNo = (.driverMobileNumber) <$> mbRide,
        vehicleNo = (.vehicleNumber) <$> mbRide,
        vehicleModel = (.vehicleModel) <$> mbRide,
        vehicleColor = mbRide >>= (.vehicleColor),
        vehicleType = (T.pack . show . (.vehicleVariant)) <$> mbRide,
        vehicleMake = Nothing,
        vehicleAppearanceNotes = Nothing,
        vehicleLat = Nothing,
        vehicleLon = Nothing,
        vehicleLocationUrl = trackLink,
        videoPath = Nothing,
        emergencyContact1Name = ec1Name,
        emergencyContact1Phone = ec1Phone,
        emergencyContact2Name = ec2Name,
        emergencyContact2Phone = ec2Phone,
        city = Just $ T.pack $ show merchantOpCity.city,
        emergencyMessage = Just "SOS Emergency",
        email = emailText,
        vendorName = Just merchantOpCity.merchantShortId.getShortId,
        deviceType = Nothing
      }

resolveLocation :: Maybe Maps.LatLong -> Person.Person -> Maybe DRide.Ride -> Flow (Double, Double)
resolveLocation mbCustomerLoc person mbRide =
  case mbCustomerLoc of
    Just loc -> pure (loc.lat, loc.lon)
    Nothing -> case (person.latestLat, person.latestLon) of
      (Just pLat, Just pLon) -> pure (pLat, pLon)
      _ -> do
        mbDriverLoc <- case mbRide of
          Just ride -> do
            resp <- withTryCatch "getDriverLoc:buildExternalSOSDetails" $ SRide.getDriverLoc ride.id
            case resp of
              Right driverLoc -> pure $ Just (driverLoc.lat, driverLoc.lon)
              Left err -> do
                logError $ "Driver location fetch failed, falling back to ride fromLocation: " <> show err
                pure Nothing
          Nothing -> pure Nothing
        case mbDriverLoc of
          Just (dLat, dLon) -> pure (dLat, dLon)
          Nothing -> pure $ maybe (0.0, 0.0) (\ride -> (ride.fromLocation.lat, ride.fromLocation.lon)) mbRide

----------------------------------------------------------------------
-- HasSosHandle instance + callback impls (Step 9 of SOS unification).
-- The old handler functions above become dead code (shell routes to
-- shared-services handlers). Step 11 of the plan deletes them.
----------------------------------------------------------------------

instance SafetySos.HasSosHandle AppEnv Environment.Flow where
  getSosHandle = do
    trackingRL <- asks (.sosTrackingRateLimitOptions)
    erssRL <- asks (.erssStatusUpdateRateLimitOptions)
    pure
      SafetySos.SosServiceHandle
        { SafetySos.getPlatformConfig =
            SafetySos.PlatformConfig
              { SafetySos.enableFollowRide = True,
                SafetySos.enableShareRide = True,
                SafetySos.enableMockDrill = True,
                SafetySos.enableExternalSos = True,
                SafetySos.enableIvr = True,
                SafetySos.enablePoliceCall = True,
                SafetySos.sosTrackingRateLimitOptions = trackingRL,
                SafetySos.erssStatusUpdateRateLimitOptions = erssRL
              },
          SafetySos.getPersonData = riderGetPersonData,
          SafetySos.getRideCtx = riderGetRideCtx,
          SafetySos.enableFollowRideForContacts = riderEnableFollowRide,
          SafetySos.sendSosNotification = riderSendSosNotification,
          SafetySos.callKaptureCreateTicket = riderCallKaptureCreateTicket,
          SafetySos.callKaptureUpdateTicket = riderCallKaptureUpdateTicket,
          SafetySos.triggerExternalSos = riderTriggerExternalSos,
          SafetySos.sendExternalSosTrace = sendExternalSOSTrace,
          SafetySos.uploadExternalSosMedia = riderUploadExternalSosMedia,
          SafetySos.registerSosWithLts = riderRegisterSosWithLts,
          SafetySos.processIvrOutcome = riderProcessIvrOutcome,
          SafetySos.triggerPoliceCall = riderTriggerPoliceCall,
          SafetySos.buildSosTrackingUrl = riderBuildSosTrackingUrl,
          SafetySos.getSosRideDetails = riderGetSosRideDetails
        }

-- | Build SosPersonData from the rider's local Person + RiderConfig + merchant
-- data. Absorbs what used to be inline person/config fetches in each handler.
riderGetPersonData :: Id SafetyCommon.Person -> Environment.Flow SafetySos.SosPersonData
riderGetPersonData sharedPersonId = do
  let personId = cast @SafetyCommon.Person @Person.Person sharedPersonId
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  riderConfig <-
    getConfig (RiderDimensions person.merchantOperatingCityId.getId)
      >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  merchantConfig <-
    CQM.findById person.merchantId
      >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  merchantOpCity <-
    CQMOC.findById person.merchantOperatingCityId
      >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  mbMobile <- mapM decrypt person.mobileNumber
  let cityCode = case A.toJSON merchantOpCity.city of
        A.String code -> code
        _ -> show merchantOpCity.city
  pure
    SafetySos.SosPersonData
      { SafetySos.personId = sharedPersonId,
        SafetySos.personName = SLP.getName person,
        SafetySos.merchantId = cast person.merchantId,
        SafetySos.merchantOperatingCityId = cast person.merchantOperatingCityId,
        SafetySos.merchantShortId = merchantConfig.shortId.getShortId,
        SafetySos.personCityCode = cityCode,
        SafetySos.personMobile = mbMobile,
        SafetySos.enableSupportForSafety = riderConfig.enableSupportForSafety,
        SafetySos.kaptureDisposition = riderConfig.kaptureConfig.disposition,
        SafetySos.kaptureQueue = fromMaybe riderConfig.kaptureConfig.queue riderConfig.kaptureConfig.sosQueue,
        SafetySos.ticketClassification = Ticket.CUSTOMER,
        SafetySos.personDisplayName = SLP.getName person,
        SafetySos.trackingUrlPattern = riderConfig.sosTrackingLink,
        SafetySos.shareRideTrackingUrlPattern = riderConfig.trackingShortUrlPattern,
        SafetySos.dashboardMediaUrlPattern = riderConfig.dashboardMediaFileUrlPattern,
        SafetySos.mediaFileSizeLimit = fromIntegral riderConfig.videoFileSizeUpperLimit,
        SafetySos.mediaFileUrlPattern = merchantConfig.mediaFileUrlPattern,
        SafetySos.externalSOSConfig = toSafetyExternalSOSConfig <$> riderConfig.externalSOSConfig,
        SafetySos.timeDiffFromUtc = riderConfig.timeDiffFromUtc,
        SafetySos.safetySettingsDefaults = Just (SLP.riderPersonToSafetySettingsPersonDefaults person)
      }

-- | Build SosRideCtx from rider's local Ride + Booking. ownerPersonId is the
-- booking's riderId — what we validate against in shared handlers.
riderGetRideCtx :: Id SafetyCommon.Ride -> Environment.Flow SafetySos.SosRideCtx
riderGetRideCtx sharedRideId = do
  let rideIdApp = cast @SafetyCommon.Ride @DRide.Ride sharedRideId
  ride <- QRide.findById rideIdApp >>= fromMaybeM (RideDoesNotExist rideIdApp.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  person <-
    QP.findById booking.riderId
      >>= fromMaybeM (PersonDoesNotExist booking.riderId.getId)
  rideMoc <-
    CQMOC.findById (fromMaybe person.merchantOperatingCityId ride.merchantOperatingCityId)
      >>= fromMaybeM (MerchantOperatingCityNotFound (fromMaybe person.merchantOperatingCityId ride.merchantOperatingCityId).getId)
  phoneNumber <- mapM decrypt person.mobileNumber
  let rideCityCode = case A.toJSON rideMoc.city of
        A.String code -> code
        _ -> show rideMoc.city
  pure
    SafetySos.SosRideCtx
      { SafetySos.rideId = sharedRideId,
        SafetySos.rideShortId = Kernel.Types.Id.ShortId ride.shortId.getShortId,
        SafetySos.ownerPersonId = cast booking.riderId,
        SafetySos.rideEndTime = ride.rideEndTime,
        SafetySos.tripEndTime = Nothing,
        SafetySos.driverName = Just ride.driverName,
        SafetySos.driverMobile = Just ride.driverMobileNumber,
        SafetySos.vehicleNumber = Just ride.vehicleNumber,
        SafetySos.vehicleModel = Just ride.vehicleModel,
        SafetySos.vehicleColor = ride.vehicleColor,
        SafetySos.vehicleVariant = Just (T.pack (show ride.vehicleVariant)),
        SafetySos.fromLat = ride.fromLocation.lat,
        SafetySos.fromLon = ride.fromLocation.lon,
        SafetySos.trackLink = "",
        SafetySos.rideInfo = Just (SIVR.buildRideInfo ride person phoneNumber),
        SafetySos.rideCityCode = rideCityCode
      }

-- | Enable "follow ride" for each contact that has a contactPersonId.
-- Reuses the existing enableFollowRideInSos helper; just adapts the contact type.
riderEnableFollowRide :: Id SafetyCommon.Person -> [SafetySos.SosEmergencyContact] -> Environment.Flow ()
riderEnableFollowRide sharedPersonId contacts = do
  let personId = cast @SafetyCommon.Person @Person.Person sharedPersonId
  mapM_
    ( \contact -> case contact.contactPersonId of
        Nothing -> pure ()
        Just cid -> do
          let appContactId = cast @SafetyCommon.Person @Person.Person cid
          contactPersonEntity <-
            QP.findById appContactId
              >>= fromMaybeM (PersonDoesNotExist appContactId.getId)
          DFR.updateFollowDetails personId contactPersonEntity
    )
    contacts

-- | Dispatch to either the "keyed" notification path or the body/title path.
-- Fetches the Person row internally because SPDEN functions expect it.
-- If SosAlertParams is provided, build the SMS function here.
riderSendSosNotification ::
  SafetySos.SosPersonData ->
  Either Text (Text, Text) ->
  Notification.Category ->
  [(Text, Text)] ->
  Maybe SafetySos.SosAlertParams ->
  Bool ->
  [SafetySos.SosEmergencyContact] ->
  Maybe (Id SafetyDSos.Sos) ->
  Environment.Flow ()
riderSendSosNotification personData keyOrBodyTitle notificationType templateVars mbAlertParams useSmsFallback contacts mbSosId = do
  let appPersonId = cast @SafetyCommon.Person @Person.Person personData.personId
  person <- QP.findById appPersonId >>= fromMaybeM (PersonDoesNotExist appPersonId.getId)
  mbSmsBuilder <- case mbAlertParams of
    Nothing -> pure Nothing
    Just alertParams -> do
      builder <-
        MessageBuilder.buildSOSAlertMessage
          person.merchantOperatingCityId
          MessageBuilder.BuildSOSAlertMessageReq
            { userName = alertParams.userName,
              rideLink = alertParams.rideLink,
              rideEndTime = alertParams.rideEndTime,
              isRideEnded = alertParams.isRideEnded
            }
      pure (Just builder)
  let pdenContacts = fmap toPdenContact contacts
  case keyOrBodyTitle of
    Left key ->
      SPDEN.notifyEmergencyContactsWithKey
        person
        key
        notificationType
        templateVars
        mbSmsBuilder
        useSmsFallback
        pdenContacts
        mbSosId
    Right (body, title) ->
      SPDEN.notifyEmergencyContacts
        person
        body
        title
        notificationType
        mbSmsBuilder
        useSmsFallback
        pdenContacts
        mbSosId
  where
    toPdenContact :: SafetySos.SosEmergencyContact -> EmergencyAPI.EmergencyContact
    toPdenContact c =
      EmergencyAPI.EmergencyContact
        { mobileNumber = c.mobileNumber,
          mobileCountryCode = c.mobileCountryCode,
          name = c.name,
          contactPersonId = cast <$> c.contactPersonId,
          priority = 0,
          enableForFollowing = False,
          enableForShareRide = False,
          shareTripWithEmergencyContactOption = Nothing,
          onRide = Nothing,
          merchantId = Just personData.merchantId
        }

riderCallKaptureCreateTicket ::
  Id SafetyCommon.Merchant ->
  Id SafetyCommon.MerchantOperatingCity ->
  Ticket.CreateTicketReq ->
  Environment.Flow (Maybe Text)
riderCallKaptureCreateTicket sharedMId sharedMocId req =
  SafetySos.wrapKaptureCreateTicket $
    Ticket.createTicket (cast @SafetyCommon.Merchant @Merchant.Merchant sharedMId) (cast @SafetyCommon.MerchantOperatingCity @DMOC.MerchantOperatingCity sharedMocId) req

riderCallKaptureUpdateTicket ::
  Id SafetyCommon.Merchant ->
  Id SafetyCommon.MerchantOperatingCity ->
  Ticket.UpdateTicketReq ->
  Environment.Flow ()
riderCallKaptureUpdateTicket sharedMId sharedMocId req =
  SafetySos.wrapKaptureUpdateTicket $
    Ticket.updateTicket (cast @SafetyCommon.Merchant @Merchant.Merchant sharedMId) (cast @SafetyCommon.MerchantOperatingCity @DMOC.MerchantOperatingCity sharedMocId) req

-- | Wraps rider's existing handleExternalSOS. The shared handler already
-- checks triggerSource == FRONTEND before calling this.
riderTriggerExternalSos ::
  SafetySos.SosPersonData ->
  SosReq ->
  Maybe SafetySos.SosRideCtx ->
  Environment.Flow SafetySos.ExternalSosResult
riderTriggerExternalSos personData req _mbRideCtx = do
  case personData.externalSOSConfig of
    Nothing -> pure $ SafetySos.ExternalSosResult Nothing Nothing Nothing
    Just safetyCfg -> do
      let appPersonId = cast @SafetyCommon.Person @Person.Person personData.personId
      person <- QP.findById appPersonId >>= fromMaybeM (PersonDoesNotExist appPersonId.getId)
      (mbRef, mbSvcCfg) <- handleExternalSOS person personData safetyCfg req
      pure $
        SafetySos.ExternalSosResult
          { externalReferenceId = mbRef,
            serviceConfig = mbSvcCfg,
            -- Rider's pre-unification behavior: set status to Just True whenever
            -- handleExternalSOS was actually invoked, regardless of whether the
            -- downstream PoliceSOS.sendInitialSOS succeeded. Preserved verbatim so
            -- SosRes.externalSOSSuccess wire shape is unchanged.
            apiCallSucceeded = Just True
          }

-- | Upload a media file to the external SOS vendor (ERSS/police).
-- Shared-services pre-checks mediaRequired / externalReferenceId / mobile.
riderUploadExternalSosMedia ::
  SafetySos.SosPersonData ->
  Id SafetyDSos.Sos ->
  Text ->
  FilePath ->
  Text ->
  Environment.Flow ()
riderUploadExternalSosMedia personData sosId phoneNo payload fileName = do
  case personData.externalSOSConfig of
    Nothing -> pure ()
    Just sosCfg -> do
      let sosServiceType = SafetyCommon.flowToSOSService sosCfg.flow
          appMId = cast @SafetyCommon.Merchant @Merchant.Merchant personData.merchantId
          appMocId = cast @SafetyCommon.MerchantOperatingCity @DMOC.MerchantOperatingCity personData.merchantOperatingCityId
      mbSpecificConfig <- getSosSpecificConfig appMId appMocId sosServiceType
      whenJust mbSpecificConfig $ \specificConfig -> do
        uploadMediaRes <- PoliceSOS.uploadMedia specificConfig phoneNo fileName payload
        if uploadMediaRes.success
          then logInfo $ "PoliceSOS:uploadMedia success; sosId=" <> sosId.getId <> ", fileName=" <> fileName
          else logError $ "PoliceSOS:uploadMedia failure; sosId=" <> sosId.getId <> ", fileName=" <> fileName <> ", errorMessage=" <> show uploadMediaRes.errorMessage

-- | Build the LTS broadcaster config (ERSS-specific) from AppEnv + args, then
-- fork LTS.entityUpsertForSos. If no external SOS is active, still fork the
-- upsert without a broadcaster config (matches rider's pre-unification path).
riderRegisterSosWithLts ::
  Id SafetyDSos.Sos ->
  SafetySos.SosPersonData ->
  Maybe Text ->
  Maybe SOSInterface.SOSServiceConfig ->
  Environment.Flow ()
riderRegisterSosWithLts sosId personData mbExternalRef mbSvcCfg = do
  let appPersonId = cast @SafetyCommon.Person @Person.Person personData.personId
      appMerchantId = cast @SafetyCommon.Merchant @Merchant.Merchant personData.merchantId
      appMocId = cast @SafetyCommon.MerchantOperatingCity @DMOC.MerchantOperatingCity personData.merchantOperatingCityId
  mbBroadcasterConfig <- case (mbExternalRef, mbSvcCfg, personData.personMobile) of
    (Just refId, Just (SOSInterface.ERSSConfig erssCfg), Just mobileNo) ->
      Just <$> buildSosErssConfigReq refId erssCfg appMocId (personData.externalSOSConfig >>= (.tracePollingIntervalSeconds)) personData.timeDiffFromUtc mobileNo
    _ -> pure Nothing
  fork "ltsSosEntityUpsert" $
    LTS.entityUpsertForSos sosId.getId appPersonId.getId appMerchantId.getId mbBroadcasterConfig

-- | Process an Exotel IVR webhook response.
-- Duplicates the old getSosIvrOutcome flow: look up the call record, update
-- the digit pressed, and (if "not 1") escalate to police via riderTriggerPoliceCall.
riderProcessIvrOutcome ::
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Environment.Flow APISuccess.APISuccess
riderProcessIvrOutcome mbCallFrom mbCallSid mbCallStatus mbDigits = do
  logDebug $ "IVR response from Exotel: " <> show mbCallSid <> ", " <> show mbDigits <> ", " <> show mbCallStatus <> ", " <> show mbCallFrom
  when (isNothing mbCallSid) $ throwError CallSidNullError
  let callSid = fromMaybe "" mbCallSid
      validStatus = fromMaybe Call.INVALID_STATUS $ (A.decode . A.encode) =<< mbCallStatus
  mbRecord <- QCallStatus.findByCallId callSid
  case mbRecord of
    Just res -> processRecord callSid mbDigits validStatus res
    Nothing -> throwError $ CallRecordNotFoundError callSid
  pure APISuccess.Success
  where
    processRecord :: Text -> Maybe Text -> Call.CallStatus -> DCall.CallStatus -> Environment.Flow ()
    processRecord callSid mbDigitPressed validStatus res = do
      let digitPressed = fromMaybe "1" $ T.replace "\"" "" <$> mbDigitPressed
      QCallStatus.updateCustomerIvrResponse callSid (Just digitPressed) validStatus
      logDebug $ "digitPressed : " <> digitPressed
      case res.rideId of
        Just rideId ->
          if digitPressed /= "1"
            then escalate rideId
            else do
              logDebug $ "Customer pressed 1; marking ride safe: " <> rideId.getId
              void $ QRide.updateSafetyJourneyStatus rideId DRide.Safe
        Nothing -> throwError $ RideIdEmptyInCallRecord callSid

    escalate :: Id DRide.Ride -> Environment.Flow ()
    escalate rideId = do
      ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
      booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
      personData <- riderGetPersonData (cast booking.riderId)
      void $ riderTriggerPoliceCall personData (cast rideId)

-- | Extracted from rider's old postSosCallPolice. Takes SosPersonData instead
-- of an auth tuple (shared handler already extracted the personId).
riderTriggerPoliceCall ::
  SafetySos.SosPersonData ->
  Id SafetyCommon.Ride ->
  Environment.Flow APISuccess.APISuccess
riderTriggerPoliceCall personData sharedRideId = do
  let appPersonId = cast @SafetyCommon.Person @Person.Person personData.personId
      appRideId = cast @SafetyCommon.Ride @DRide.Ride sharedRideId
      appMerchantId = cast @SafetyCommon.Merchant @Merchant.Merchant personData.merchantId
  person <- runInReplica $ QP.findById appPersonId >>= fromMaybeM (PersonDoesNotExist appPersonId.getId)
  ride <- runInReplica $ QRide.findById appRideId >>= fromMaybeM (RideDoesNotExist appRideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let merchantOpCityId = booking.merchantOperatingCityId
  riderConfig <-
    getConfig (RiderDimensions merchantOpCityId.getId)
      >>= fromMaybeM (RiderConfigDoesNotExist merchantOpCityId.getId)
  if riderConfig.incidentReportSupport
    then do
      coordinates <- fetchLatLong ride appMerchantId
      token <- getTokenofJMService appMerchantId merchantOpCityId
      incidentReportHandler person appMerchantId merchantOpCityId ride.id token coordinates riderConfig
      void $ QRide.updateSafetyJourneyStatus ride.id DRide.PoliceMonitoring
    else do
      logDebug $ "Incident Report Support disabled; creating safety ticket for ride: " <> show ride.id
      SIVR.createSafetyTicket person ride
  pure APISuccess.Success

-- | Pure tracking-URL builder — same logic as pre-unification `buildSosTrackUrl`.
-- Uses `trackingUrlPattern` (rider's `sosTrackingLink`) when set, else falls
-- back to `shareRideTrackingUrlPattern` (rider's `trackingShortUrlPattern`) +
-- the ride's UUID rideId.
riderBuildSosTrackingUrl ::
  SafetySos.SosPersonData ->
  Maybe SafetySos.SosRideCtx ->
  Id SafetyDSos.Sos ->
  Text
riderBuildSosTrackingUrl personData mbRideCtx sosId =
  case personData.trackingUrlPattern of
    Just sosLink -> T.replace "{#vp#}" "sosTracking" sosLink <> sosId.getId
    Nothing ->
      T.replace "{#vp#}" "shareRide" personData.shareRideTrackingUrlPattern
        <> fromMaybe "" (fmap ((.getId) . (.rideId)) mbRideCtx)

-- | Rider stub — driver is the primary consumer of this endpoint.
riderGetSosRideDetails ::
  ShortId SafetyCommon.Ride ->
  Environment.Flow RideDetailsForDriverRes
riderGetSosRideDetails _ =
  throwError $ InvalidRequest "sosRideDetails is not supported on rider"
