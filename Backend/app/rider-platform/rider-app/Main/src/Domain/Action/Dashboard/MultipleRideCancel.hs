{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.Dashboard.MultipleRideCancel where
import qualified Domain.Types.Booking.Type as DB
import qualified Domain.Types.Ride as Domain
import Environment
import Kernel.Prelude
import Kernel.Storage.Esqueleto hiding (count, isNothing)
import Kernel.Types.Id
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Ride as QRide
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Domain.Types.BookingCancellationReason as SBCR
import Domain.Types.Ride
import Domain.Types.Booking
import qualified  Domain.Types.CancellationReason as DTCR




data RideCancelInfo = RideCancelInfo 
    {   rideId :: Id Ride,
        bookingId :: Id Booking,
        cancellationReasonCode :: DTCR.CancellationReasonCode,
        cancellationStage :: DTCR.CancellationStage,
        additionalInfo :: Maybe Text
    }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
newtype MultipleRideCancelReq = MultipleRideCancelReq
  { multipleRideCancelInfo :: [RideCancelInfo]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)



instance HideSecrets MultipleRideCancelReq where
  hideSecrets = identity

class ToJSON (ReqWithoutSecrets req) => HideSecrets req where
  type ReqWithoutSecrets req
  hideSecrets :: req -> ReqWithoutSecrets req
  type ReqWithoutSecrets req = req

-- FIXME next default implementation is not working
-- default hideSecrets :: req -> req
-- hideSecrets = identity

instance HideSecrets () where
  hideSecrets = identity



  
updateRideAndBookingStatus ::
  (EsqDBFlow m r)=>
  RideCancelInfo ->
  m ()
updateRideAndBookingStatus info = do
  cancellationReason <- buildBookingCancelationReason
  runTransaction $ do
    QRide.updateStatus info.rideId  Domain.CANCELLED
    QRB.updateStatus info.bookingId DB.CANCELLED
    QBCR.upsert cancellationReason
  where
  buildBookingCancelationReason = do
    return $
      SBCR.BookingCancellationReason
        { bookingId = info.bookingId,
          rideId = Just info.rideId,
          source = SBCR.ByMerchant,
          reasonCode = Just info.cancellationReasonCode,
          reasonStage = Just info.cancellationStage,
          additionalInfo = info.additionalInfo
        }

multipleRideCancel ::
  -- ShortId DM.Merchant ->
  MultipleRideCancelReq ->
  Flow APISuccess
multipleRideCancel req = do
  _ <- mapM updateRideAndBookingStatus req.multipleRideCancelInfo
  pure Success