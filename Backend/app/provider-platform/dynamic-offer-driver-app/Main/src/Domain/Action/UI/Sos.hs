{-# OPTIONS_GHC -Wwarn=unused-imports -Wno-orphans #-}

-- | SOS helpers + `HasSosHandle` instance for the driver-app platform.
-- Handler logic has moved to `Safety.Domain.Action.UI.Sos` (shared-services);
-- this module now only supplies the app-specific callbacks via the instance.
module Domain.Action.UI.Sos where

import qualified AWS.S3 as S3
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as DRideDetails
import qualified Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import EulerHS.Types (base64Encode)
import GHC.IO.Handle (hFileSize)
import GHC.IO.IOMode (IOMode (..))
import qualified IssueManagement.Domain.Types.MediaFile as DMF
import qualified IssueManagement.Storage.Queries.MediaFile as MFQuery
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification as Notification
import Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude (ToSchema, showBaseUrl)
import qualified Kernel.Prelude
import Kernel.ServantMultipart
import qualified Kernel.Types.APISuccess
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions (..))
import Kernel.Utils.Common
import Kernel.Utils.Logging
import Kernel.Utils.Servant.Client (withShortRetry)
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import qualified Safety.API.Types.UI.Sos
import qualified Safety.API.Types.UI.Sos as APISos
import qualified Safety.Domain.Action.UI.Sos as SafetySos
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.Sos
import qualified Safety.Domain.Types.Sos as SafetyDSos
import qualified Safety.Storage.CachedQueries.Sos as SafetyCQSos
import qualified Safety.Storage.Queries.SafetySettingsExtra as QSafetyExtra
import Servant hiding (throwError)
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.PersonDefaultEmergencyNumber as SPDEN
import Storage.Beam.IssueManagement ()
import Storage.Beam.Sos ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideDetails
import qualified Storage.Queries.RiderDetails as QRiderDetails
import Tools.Auth
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Ticket as TicketTools

driverGetName :: Person.Person -> Text
driverGetName person = person.firstName <> " " <> fromMaybe "" person.lastName

buildRideInfo :: DRide.Ride -> DBooking.Booking -> DRideDetails.RideDetails -> Person.Person -> Maybe Text -> Maybe Text -> Ticket.RideInfo
buildRideInfo ride booking rideDetails person driverPhoneNumber customerPhoneNumber =
  Ticket.RideInfo
    { rideShortId = ride.shortId.getShortId,
      rideCity = show person.merchantOperatingCityId,
      customerName = booking.riderName,
      customerPhoneNo = customerPhoneNumber,
      driverName = Just $ driverGetName person,
      driverPhoneNo = driverPhoneNumber,
      vehicleNo = rideDetails.vehicleNumber,
      vehicleCategory = show <$> ride.vehicleVariant,
      vehicleServiceTier = ride.vehicleServiceTierName,
      status = show ride.status,
      rideCreatedAt = ride.createdAt,
      pickupLocation = castLocation ride.fromLocation,
      dropLocation = castLocation <$> ride.toLocation,
      fare = Nothing
    }
  where
    castLocation loc =
      Ticket.Location
        { lat = loc.lat,
          lon = loc.lon,
          street = loc.address.street,
          city = loc.address.city,
          state = loc.address.state,
          country = loc.address.country,
          building = loc.address.building,
          areaCode = loc.address.areaCode,
          area = loc.address.area
        }
getSosRideDetails :: (Kernel.Types.Id.ShortId SafetyCommon.Ride -> Environment.Flow Safety.API.Types.UI.Sos.RideDetailsForDriverRes)
getSosRideDetails rideShortIdShared = do
  checkSlidingWindowLimitWithOptions ("SosRideDetails:rateLimit:" <> rideShortIdShared.getShortId) (APIRateLimitOptions {limit = 20, limitResetTimeInSec = 60})
  let rideShortId = Kernel.Types.Id.ShortId rideShortIdShared.getShortId :: Kernel.Types.Id.ShortId DRide.Ride
  ride <- QRide.findByShortId rideShortId >>= fromMaybeM (RideDoesNotExist rideShortId.getShortId)
  rideDetails <- QRideDetails.findById ride.id >>= fromMaybeM (InvalidRequest $ "RideDetailsNotFound: " <> ride.id.getId)
  return
    APISos.RideDetailsForDriverRes
      { trackingUrl = showBaseUrl ride.trackingUrl,
        driverName = rideDetails.driverName,
        tripStartPos = ride.tripStartPos,
        tripEndPos = ride.tripEndPos,
        vehicleNumber = rideDetails.vehicleNumber,
        vehicleVariant = T.pack . show <$> rideDetails.vehicleVariant
      }

----------------------------------------------------------------------
-- HasSosHandle instance + callback impls (Step 10 of SOS unification).
-- Driver's handle is mostly stubs — most SOS features are rider-only.
-- See §4 of the plan doc for the deliberate behavior changes.
----------------------------------------------------------------------

instance SafetySos.HasSosHandle Environment.AppEnv Environment.Flow where
  getSosHandle =
    pure
      SafetySos.SosServiceHandle
        { SafetySos.getPlatformConfig =
            SafetySos.PlatformConfig
              { SafetySos.enableFollowRide = False,
                SafetySos.enableShareRide = False,
                SafetySos.enableMockDrill = False,
                SafetySos.enableExternalSos = False,
                SafetySos.enableIvr = False,
                SafetySos.enablePoliceCall = False,
                -- Permissive (no-limit in practice) since driver gates most
                -- rate-limited endpoints with enableShareRide = False anyway.
                SafetySos.sosTrackingRateLimitOptions = APIRateLimitOptions {limit = 1000000, limitResetTimeInSec = 60},
                SafetySos.erssStatusUpdateRateLimitOptions = APIRateLimitOptions {limit = 1000000, limitResetTimeInSec = 60}
              },
          SafetySos.getPersonData = driverGetPersonData,
          SafetySos.getRideCtx = driverGetRideCtx,
          SafetySos.enableFollowRideForContacts = \_ _ -> pure (),
          SafetySos.sendSosNotification = driverSendSosNotification,
          SafetySos.callKaptureCreateTicket = driverCallKaptureCreateTicket,
          SafetySos.callKaptureUpdateTicket = driverCallKaptureUpdateTicket,
          SafetySos.triggerExternalSos = \_ _ _ ->
            pure $
              SafetySos.ExternalSosResult
                { SafetySos.externalReferenceId = Nothing,
                  SafetySos.serviceConfig = Nothing,
                  SafetySos.apiCallSucceeded = Nothing
                },
          SafetySos.sendExternalSosTrace = \_ _ -> pure (),
          SafetySos.uploadExternalSosMedia = \_ _ _ _ _ -> pure (),
          SafetySos.registerSosWithLts = \_ _ _ _ -> pure (),
          SafetySos.processIvrOutcome = \_ _ _ _ ->
            throwError $ InvalidRequest "IVR is not supported on this platform",
          SafetySos.triggerPoliceCall = \_ _ ->
            throwError $ InvalidRequest "Police call is not supported on this platform",
          SafetySos.buildSosTrackingUrl = driverBuildSosTrackingUrl,
          SafetySos.getSosRideDetails = getSosRideDetails
        }

driverGetPersonData :: Id SafetyCommon.Person -> Environment.Flow SafetySos.SosPersonData
driverGetPersonData sharedPersonId = do
  let personId = cast @SafetyCommon.Person @Person.Person sharedPersonId
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  transporterConfig <-
    SCTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing
      >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId)
  merchantConfig <-
    CQM.findById person.merchantId
      >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
  merchantOpCity <-
    CQMOC.findById person.merchantOperatingCityId
      >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  mbMobile <- mapM decrypt person.mobileNumber
  let cityCode = case A.toJSON merchantOpCity.city of
        A.String code -> code
        _ -> T.pack (show merchantOpCity.city)
  pure
    SafetySos.SosPersonData
      { SafetySos.personId = sharedPersonId,
        SafetySos.personName = driverGetName person,
        SafetySos.merchantId = cast person.merchantId,
        SafetySos.merchantOperatingCityId = cast person.merchantOperatingCityId,
        SafetySos.merchantShortId = merchantConfig.shortId.getShortId,
        SafetySos.personCityCode = cityCode,
        SafetySos.personMobile = mbMobile,
        SafetySos.enableSupportForSafety = fromMaybe False transporterConfig.enableSupportForSafety,
        SafetySos.kaptureDisposition = transporterConfig.kaptureDisposition,
        SafetySos.kaptureQueue = transporterConfig.kaptureQueue,
        SafetySos.ticketClassification = Ticket.DRIVER,
        SafetySos.personDisplayName = driverGetName person,
        SafetySos.trackingUrlPattern = Nothing,
        SafetySos.shareRideTrackingUrlPattern = fromMaybe "" transporterConfig.trackingShortUrlPattern,
        SafetySos.dashboardMediaUrlPattern = transporterConfig.dashboardMediaFileUrlPattern,
        SafetySos.mediaFileSizeLimit = fromIntegral transporterConfig.mediaFileSizeUpperLimit,
        SafetySos.mediaFileUrlPattern = transporterConfig.mediaFileUrlPattern,
        SafetySos.externalSOSConfig = Nothing,
        SafetySos.timeDiffFromUtc = 0,
        SafetySos.safetySettingsDefaults = Nothing
      }

driverGetRideCtx :: Id SafetyCommon.Ride -> Environment.Flow SafetySos.SosRideCtx
driverGetRideCtx sharedRideId = do
  let rideIdApp = cast @SafetyCommon.Ride @DRide.Ride sharedRideId
  ride <- QRide.findById rideIdApp >>= fromMaybeM (RideDoesNotExist rideIdApp.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  rideDetails <- QRideDetails.findById ride.id >>= fromMaybeM (InvalidRequest $ "RideDetailsNotFound: " <> ride.id.getId)
  driverPerson <- QP.findById ride.driverId >>= fromMaybeM (PersonDoesNotExist ride.driverId.getId)
  driverPhone <- mapM decrypt driverPerson.mobileNumber
  customerPhone <- case booking.riderId of
    Nothing -> pure Nothing
    Just riderDetailsId -> do
      riderDetails <-
        QRiderDetails.findById riderDetailsId
          >>= fromMaybeM (RiderDetailsNotFound riderDetailsId.getId)
      mobile <- decrypt riderDetails.mobileNumber
      pure $ Just (riderDetails.mobileCountryCode <> mobile)
  let rideMocId = ride.merchantOperatingCityId
  rideMoc <-
    CQMOC.findById rideMocId
      >>= fromMaybeM (MerchantOperatingCityNotFound rideMocId.getId)
  let rideCityCode = case A.toJSON rideMoc.city of
        A.String code -> code
        _ -> T.pack (show rideMoc.city)
      rideInfo = buildRideInfo ride booking rideDetails driverPerson driverPhone customerPhone
  pure
    SafetySos.SosRideCtx
      { SafetySos.rideId = sharedRideId,
        SafetySos.rideShortId = Kernel.Types.Id.ShortId ride.shortId.getShortId,
        SafetySos.ownerPersonId = cast ride.driverId,
        SafetySos.rideEndTime = Nothing,
        SafetySos.tripEndTime = ride.tripEndTime,
        SafetySos.driverName = Just rideDetails.driverName,
        SafetySos.driverMobile = driverPhone,
        SafetySos.vehicleNumber = Just rideDetails.vehicleNumber,
        SafetySos.vehicleModel = Nothing,
        SafetySos.vehicleColor = Nothing,
        SafetySos.vehicleVariant = T.pack . show <$> rideDetails.vehicleVariant,
        SafetySos.fromLat = ride.fromLocation.lat,
        SafetySos.fromLon = ride.fromLocation.lon,
        SafetySos.trackLink = "",
        SafetySos.rideInfo = Just rideInfo,
        SafetySos.rideCityCode = rideCityCode
      }

-- | Driver's notification callback. Only the `Left key` variant is actually
-- reachable — mock drill (which uses `Right (body, title)`) is gated off on
-- driver via enableMockDrill = False, so the Right branch throws defensively.
driverSendSosNotification ::
  SafetySos.SosPersonData ->
  Either Text (Text, Text) ->
  Notification.Category ->
  [(Text, Text)] ->
  Maybe SafetySos.SosAlertParams ->
  Bool ->
  [SafetySos.SosEmergencyContact] ->
  Maybe (Id SafetyDSos.Sos) ->
  Environment.Flow ()
driverSendSosNotification personData keyOrBodyTitle notificationType templateVars mbAlertParams useSmsFallback contacts mbSosId = do
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
  let driverContacts = fmap toDriverContact contacts
  case keyOrBodyTitle of
    Left key ->
      SPDEN.notifyEmergencyContactsWithKey
        person
        key
        notificationType
        templateVars
        mbSmsBuilder
        useSmsFallback
        driverContacts
        mbSosId
    Right _ ->
      throwError $ InternalError "Driver does not support body/title notifications"
  where
    toDriverContact :: SafetySos.SosEmergencyContact -> SPDEN.DriverEmergencyContactEntity
    toDriverContact c =
      SPDEN.DriverEmergencyContactEntity
        { name = c.name,
          mobileCountryCode = c.mobileCountryCode,
          mobileNumber = c.mobileNumber,
          contactPersonId = cast <$> c.contactPersonId
        }

driverCallKaptureCreateTicket ::
  Id SafetyCommon.Merchant ->
  Id SafetyCommon.MerchantOperatingCity ->
  Ticket.CreateTicketReq ->
  Environment.Flow (Maybe Text)
driverCallKaptureCreateTicket sharedMId sharedMocId req = do
  let mId = cast @SafetyCommon.Merchant @Domain.Types.Merchant.Merchant sharedMId
      mocId = cast @SafetyCommon.MerchantOperatingCity @Domain.Types.MerchantOperatingCity.MerchantOperatingCity sharedMocId
  result <- withTryCatch "createTicket:sos" $ TicketTools.createTicket mId mocId req
  pure $ case result of
    Right r -> Just r.ticketId
    Left _ -> Nothing

driverCallKaptureUpdateTicket ::
  Id SafetyCommon.Merchant ->
  Id SafetyCommon.MerchantOperatingCity ->
  Ticket.UpdateTicketReq ->
  Environment.Flow ()
driverCallKaptureUpdateTicket sharedMId sharedMocId req = do
  let mId = cast @SafetyCommon.Merchant @Domain.Types.Merchant.Merchant sharedMId
      mocId = cast @SafetyCommon.MerchantOperatingCity @Domain.Types.MerchantOperatingCity.MerchantOperatingCity sharedMocId
  fork "updateTicket:sos" $ void $ TicketTools.updateTicket mId mocId req

-- | Driver's tracking URL uses the ride's shortId (not UUID rideId).
-- Falls back to the hardcoded nammayatri URL when transporterConfig has no
-- `trackingShortUrlPattern` (empty string sentinel).
driverBuildSosTrackingUrl ::
  SafetySos.SosPersonData ->
  Maybe SafetySos.SosRideCtx ->
  Id SafetyDSos.Sos ->
  Text
driverBuildSosTrackingUrl personData mbRideCtx _sosId =
  case mbRideCtx of
    Nothing -> ""
    Just ctx ->
      let shortIdText = ctx.rideShortId.getShortId
       in if T.null personData.shareRideTrackingUrlPattern
            then "https://nammayatri.in/p/?vp=shareRide&rideId=" <> shortIdText
            else Notify.buildTemplate [("vp", "shareRide")] personData.shareRideTrackingUrlPattern <> shortIdText
