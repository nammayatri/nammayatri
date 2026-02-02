{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.SafetyCSAlert where

import API.Types.UI.Sos
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt)
import Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified Safety.Domain.Action.UI.Sos as SafetySos
import qualified Safety.Domain.Types.Sos as SafetyDSos
import SharedLogic.JobScheduler
import SharedLogic.Person as SLP
import Storage.Beam.Sos ()
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QR
import Tools.Error
import qualified Tools.Notifications as Notify
import Tools.Ticket as Ticket

sendSafetyCSAlert ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r,
    SchedulerFlow r
  ) =>
  Job 'SafetyCSAlert ->
  m ExecutionResult
sendSafetyCSAlert Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      personId = jobData.personId
      rideId = jobData.rideId
  ride <- B.runInReplica $ QR.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  person <- B.runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  case ride.safetyJourneyStatus of
    Just DRide.IVRCallInitiated -> createSafetyTicket person ride
    Nothing -> logError $ "Safety Journey Status is not set for ride : " <> show rideId
    _ -> logDebug $ "Ride status is : " <> show ride.safetyJourneyStatus <> ". Skipping Create Ticket for ride : " <> show rideId
  return Complete

createSafetyTicket ::
  ( EncFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EsqDBFlow m r
  ) =>
  DP.Person ->
  DRide.Ride ->
  m ()
createSafetyTicket person ride = do
  logDebug $ "Creating Safety Ticket for ride : " <> show ride.id
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  let trackLink = Notify.buildTrackingUrl ride.id [("vp", "shareRide")] riderConfig.trackingShortUrlPattern
  phoneNumber <- mapM decrypt person.mobileNumber
  let rideInfo = buildRideInfo ride person phoneNumber
      kaptureQueue = fromMaybe riderConfig.kaptureConfig.queue riderConfig.kaptureConfig.sosQueue
  ticketResponse <- withTryCatch "createTicket:safetyCSAlert" (createTicket person.merchantId person.merchantOperatingCityId (mkTicket person phoneNumber ["https://" <> trackLink] rideInfo SafetyDSos.CSAlertSosTicket riderConfig.kaptureConfig.disposition kaptureQueue))
  ticketId <- do
    case ticketResponse of
      Right ticketResponse' -> do
        void $ QR.updateSafetyJourneyStatus ride.id DRide.CSAlerted
        logDebug $ "Ticket created when rider didn't picked up call : " <> show (Just ticketResponse'.ticketId)
        return (Just ticketResponse'.ticketId)
      Left err -> do
        logError $ "Ticket didn't created when rider didn't picked up call with error : " <> show err
        return Nothing
  sosDetails <- buildSosDetails person SosReq {flow = SafetyDSos.CSAlertSosTicket, rideId = Just ride.id, isRideEnded = Nothing, notifyAllContacts = Nothing, customerLocation = Nothing, sendPNOnPostRideSOS = Nothing} ticketId
  void $ SafetySos.createSos sosDetails

mkTicket :: DP.Person -> Maybe Text -> [Text] -> Ticket.RideInfo -> SafetyDSos.SosType -> Text -> Text -> Ticket.CreateTicketReq
mkTicket person phoneNumber mediaLinks info flow disposition queue = do
  Ticket.CreateTicketReq
    { category = "Code Red",
      subCategory = Just "SOS Alert (follow-back)",
      issueId = Nothing,
      issueDescription,
      mediaFiles = Just mediaLinks,
      name = Just $ SLP.getName person,
      phoneNo = phoneNumber,
      personId = person.id.getId,
      classification = Ticket.CUSTOMER,
      rideDescription = Just info,
      disposition,
      queue,
      becknIssueId = Nothing
    }
  where
    issueDescription = case flow of
      SafetyDSos.Police -> "112 called"
      SafetyDSos.CSAlertSosTicket -> "SOS ticket created for night safety when rider didn't picked exotel call"
      SafetyDSos.AudioRecording -> "Audio recording shared."
      SafetyDSos.CustomerCare -> "Customer care called."
      _ -> "SOS activated"

buildRideInfo :: DRide.Ride -> DP.Person -> Maybe Text -> Ticket.RideInfo
buildRideInfo ride person phoneNumber =
  Ticket.RideInfo
    { rideShortId = ride.shortId.getShortId,
      rideCity = show person.currentCity,
      customerName = Just $ SLP.getName person,
      customerPhoneNo = phoneNumber,
      driverName = Just ride.driverName,
      driverPhoneNo = Just ride.driverMobileNumber,
      vehicleNo = ride.vehicleNumber,
      vehicleCategory = Just $ show ride.vehicleVariant,
      vehicleServiceTier = show <$> ride.vehicleServiceTierType,
      status = show ride.status,
      rideCreatedAt = ride.createdAt,
      pickupLocation = castLocationAPIEntity ride.fromLocation,
      dropLocation = castLocationAPIEntity <$> ride.toLocation,
      fare = Nothing
    }
  where
    castLocationAPIEntity ent =
      Ticket.Location
        { lat = ent.lat,
          lon = ent.lon,
          street = ent.address.street,
          city = ent.address.city,
          state = ent.address.state,
          country = ent.address.country,
          building = ent.address.building,
          areaCode = ent.address.areaCode,
          area = ent.address.area
        }

buildSosDetails :: (EncFlow m r) => DP.Person -> SosReq -> Maybe Text -> m SafetyDSos.Sos
buildSosDetails person req ticketId = do
  pid <- generateGUID
  now <- getCurrentTime
  rideId <- case req.rideId of
    Just existingRideId -> return (cast existingRideId)
    Nothing -> cast . Id <$> generateGUID
  return
    SafetyDSos.Sos
      { id = pid,
        personId = cast person.id,
        status = SafetyDSos.Pending,
        flow = req.flow,
        rideId = Just rideId,
        ticketId = ticketId,
        mediaFiles = [],
        merchantId = Just (cast person.merchantId),
        merchantOperatingCityId = Just (cast person.merchantOperatingCityId),
        trackingExpiresAt = Nothing,
        entityType = Just SafetyDSos.Ride,
        sosState = Just SafetyDSos.SosActive,
        externalReferenceId = Nothing,
        externalReferenceStatus = Nothing,
        externalStatusHistory = Nothing,
        createdAt = now,
        updatedAt = now
      }
