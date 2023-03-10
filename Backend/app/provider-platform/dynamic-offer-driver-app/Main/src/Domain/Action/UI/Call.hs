{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Call
  ( CallCallbackRes,
    GetCustomerMobileNumberResp,
    directCallStatusCallback,
    getCustomerMobileNumber,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.CallStatus as SCS
import Kernel.External.Call.Exotel.Types
import Kernel.External.Call.Interface.Exotel (exotelStatusToInterfaceStatus)
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow, runInReplica, runTransaction)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error

type CallCallbackRes = AckResponse

type GetCustomerMobileNumberResp = Text

directCallStatusCallback :: EsqDBFlow m r => Text -> ExotelCallStatus -> Text -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus recordingUrl_ callDuration = do
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  recordingUrl <- parseBaseUrl recordingUrl_
  runTransaction $ QCallStatus.updateCallStatus callStatus.id (exotelStatusToInterfaceStatus dialCallStatus) (fromMaybe 0 callDuration) recordingUrl
  return Ack

getCustomerMobileNumber :: (EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Text -> Text -> ExotelCallStatus -> m GetCustomerMobileNumberResp
getCustomerMobileNumber callSid callFrom_ callStatus = do
  let callFrom = dropFirstZero callFrom_
  mobileNumberHash <- getDbHash callFrom
  driver <- runInReplica $ QPerson.findByMobileNumber "+91" mobileNumberHash >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
  activeRide <- runInReplica $ QRide.getActiveByDriverId driver.id >>= fromMaybeM (RideForDriverNotFound $ getId driver.id)
  activeBooking <- runInReplica $ QRB.findById activeRide.bookingId >>= fromMaybeM (BookingNotFound $ getId activeRide.bookingId)
  riderId <-
    activeBooking.riderId
      & fromMaybeM (BookingFieldNotPresent "riderId")
  riderDetails <-
    runInReplica $
      QRD.findById riderId
        >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
  requestorPhone <- decrypt riderDetails.mobileNumber
  callId <- generateGUID
  callStatusObj <- buildCallStatus activeRide.id callId callSid (exotelStatusToInterfaceStatus callStatus)
  runTransaction $ QCallStatus.create callStatusObj
  return requestorPhone
  where
    dropFirstZero = T.dropWhile (== '0')
    buildCallStatus rideId callId exotelCallId exoStatus = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callId,
            callId = exotelCallId,
            rideId = rideId,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }
