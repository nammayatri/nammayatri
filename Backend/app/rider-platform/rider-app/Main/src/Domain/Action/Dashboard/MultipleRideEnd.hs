{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.Dashboard.MultipleRideEnd
  ( module Domain.Action.Dashboard.MultipleRideEnd,
    module Reexport,
  )
where

import "dashboard-helper-api" Dashboard.Common as Reexport
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as SRide
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Ride as QRide
import Tools.Error

newtype RideCompletedReq = RideCompletedReq
  { bookingId :: Id SRB.Booking
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

newtype MultipleRideEndReq = MultipleRideEndReq
  { multipleRideEndInfo :: [RideCompletedReq]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets MultipleRideEndReq where
  hideSecrets = identity

bppRideEnd ::
  ( EsqDBFlow m r,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)
  ) =>
  RideCompletedReq ->
  m ()
bppRideEnd RideCompletedReq {..} = do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist $ "BookingId: " <> bookingId.getId)
  ride <- QRide.findActiveByRBId bookingId >>= fromMaybeM (RideDoesNotExist $ "bookingId" <> bookingId.getId)
  unless (booking.status == SRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
  unless (ride.status == SRide.INPROGRESS) $ throwError (RideInvalidStatus $ show ride.status)
  rideEndTime <- getCurrentTime
  let updRide =
        ride{status = SRide.COMPLETED,
             fare = ride.fare,
             totalFare = ride.totalFare,
             chargeableDistance = ride.chargeableDistance,
             rideEndTime = Just rideEndTime
            }
  person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  let shouldUpdateRideComplete =
        case minTripDistanceForReferralCfg of
          Just distance -> updRide.chargeableDistance >= Just distance && not person.hasTakenValidRide
          Nothing -> True
  DB.runTransaction $ do
    when shouldUpdateRideComplete $
      QP.updateHasTakenValidRide booking.riderId
    QRB.updateStatus booking.id SRB.COMPLETED
    QRide.updateMultiple updRide.id updRide
    QPFS.updateStatus booking.riderId DPFS.PENDING_RATING {rideId = ride.id}

multipleRideEnd ::
  MultipleRideEndReq ->
  Flow APISuccess
multipleRideEnd req = do
  _ <- mapM bppRideEnd req.multipleRideEndInfo
  pure Success
