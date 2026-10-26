-- | Driver calls the rider of a shared-cab booking through Exotel, so each side sees only the ExoPhone. Same shape as
-- Domain.Action.UI.Call.initiateCallToCustomer, but keyed by the FRFS booking id: rider-app owns the booking and the rider's phone,
-- and decides whether the booking is inside the call window.
module Domain.Action.UI.SharedCabCall (callRider, SharedCabMessageReq (..), sendMessage) where

import Data.OpenApi (ToSchema)
import Domain.Action.UI.Call (CallAttachments (..), CallRes (..), handleCallStatus)
import Domain.Action.UI.SharedCab (DriverAuthInfo, bapInternal, validateSharedCabDriver)
import qualified Domain.Types.CallStatus as SCS
import Domain.Types.Person (Person)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.SharedCab.RiderClient as RiderClient
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import Tools.Call

callRider :: DriverAuthInfo -> Text -> Flow CallRes
callRider (personId, merchantId, merchantOpCityId) bookingId = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  rider <- RiderClient.getRiderCallNumber bap.apiKey bap.url bookingId personId.getId vehicle.registrationNo
  driverPhone <- getDriverPhone personId
  callStatusId <- generateGUID
  let callReq =
        InitiateCallReq
          { fromPhoneNum = driverPhone,
            toPhoneNum = Just rider.riderNumber,
            attachments = Attachments $ CallAttachments {callStatusId = callStatusId, entityId = bookingId},
            appletId = Nothing
          }
  exotelResponse <- initiateCall merchantId merchantOpCityId callReq
  logTagInfo ("BookingId: " <> bookingId) "Call initiated from shared-cab driver to rider."
  now <- getCurrentTime
  QCallStatus.create
    SCS.CallStatus
      { id = callStatusId,
        callId = exotelResponse.callId,
        entityId = Just bookingId,
        dtmfNumberUsed = Nothing,
        status = exotelResponse.callStatus,
        conversationDuration = 0,
        recordingUrl = Nothing,
        merchantId = Just merchantId.getId,
        merchantOperatingCityId = Just merchantOpCityId,
        callService = Just Exotel,
        callAttempt = Just (handleCallStatus exotelResponse.callStatus),
        callError = Nothing,
        createdAt = now,
        aiCallAnalyzed = Nothing
      }
  pure $ CallRes callStatusId

getDriverPhone :: Id Person -> Flow Text
getDriverPhone personId = do
  driver <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  decrypted <- mapM decrypt driver.mobileNumber
  fromMaybeM (InternalError "Driver has no phone number.") $ (<>) <$> driver.mobileCountryCode <*> decrypted

newtype SharedCabMessageReq = SharedCabMessageReq {message :: Text}
  deriving (Generic, Show, Eq, ToJSON, FromJSON, ToSchema)

-- | The booking-keyed twin of POST /onMessage: rider-app checks the booking is on this driver's live cab and sends the rider the CHAT_MESSAGE push.
sendMessage :: DriverAuthInfo -> Text -> SharedCabMessageReq -> Flow APISuccess
sendMessage (personId, _, _) bookingId req = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  RiderClient.postRiderMessage bap.apiKey bap.url bookingId RiderClient.BAPMessageReq {driverId = personId.getId, vehicleNumber = vehicle.registrationNo, message = req.message}
