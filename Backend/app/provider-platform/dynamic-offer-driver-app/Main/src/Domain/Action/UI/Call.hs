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
    GetCallStatusRes,
    MobileNumberResp,
    directCallStatusCallback,
    getCustomerMobileNumber,
    getCallStatus,
  )
where

import qualified Data.Text as T
import Data.Text.Conversions
import qualified Domain.Types.CallStatus as SCS
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.External.Exotel.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto (EsqDBReplicaFlow, runInReplica, runTransaction)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as Merchant
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import Tools.Error

type CallCallbackRes = AckResponse

type GetCallStatusRes = SCS.CallStatusAPIEntity

type MobileNumberResp = Text

directCallStatusCallback :: EsqDBFlow m r => Text -> Text -> Text -> Maybe Int -> m CallCallbackRes
directCallStatusCallback callSid dialCallStatus_ recordingUrl_ callDuration = do
  let dialCallStatus = fromText dialCallStatus_ :: ExotelCallStatus
  callStatus <- QCallStatus.findByCallSid callSid >>= fromMaybeM CallStatusDoesNotExist
  recordingUrl <- parseBaseUrl recordingUrl_
  runTransaction $ QCallStatus.updateCallStatus callStatus.id dialCallStatus (fromMaybe 0 callDuration) recordingUrl
  return Ack

getCustomerMobileNumber :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Text -> Text -> Text -> Text -> m MobileNumberResp
getCustomerMobileNumber callSid callFrom_ callTo_ callStatus_ = do
  let callStatus = fromText callStatus_ :: ExotelCallStatus
  let callFrom = dropFirstZero callFrom_
  let callTo = dropFirstZero callTo_
  mobileNumberHash <- getDbHash callFrom
  merchant <- Merchant.findAllByExoPhone "+91" callTo >>= fromMaybeM (MerchantWithExoPhoneNotFound callTo)
  driver <- runInReplica $ QPerson.findByMobileNumberAndMerchant "+91" mobileNumberHash merchant.id >>= fromMaybeM (PersonWithPhoneNotFound callFrom)
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
  callStatusObj <- buildCallStatus activeRide.id callId callSid callStatus
  runTransaction $ QCallStatus.create callStatusObj
  return requestorPhone
  where
    dropFirstZero = T.dropWhile (== '0')
    buildCallStatus rideId callId exotelCallId exoStatus = do
      now <- getCurrentTime
      return $
        SCS.CallStatus
          { id = callId,
            exotelCallSid = exotelCallId,
            rideId = rideId,
            status = exoStatus,
            conversationDuration = 0,
            recordingUrl = Nothing,
            createdAt = now
          }

getCallStatus :: (EsqDBFlow m r) => Id SCS.CallStatus -> m GetCallStatusRes
getCallStatus callStatusId = do
  QCallStatus.findById callStatusId >>= fromMaybeM CallStatusDoesNotExist <&> SCS.makeCallStatusAPIEntity
