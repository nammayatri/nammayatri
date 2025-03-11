{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.QueriesExtra.RideLite where

import qualified Domain.Types
import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.Ride as Beam

---------------- Use this function if you need the data which are here as per your domain requirement ----------------
---------------- Add items in the below domain according to your use                                  ----------------

findByIdLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id RideLite -> m (Maybe RideLite))
findByIdLite id = findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByRBIdLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe RideLite))
findByRBIdLite bookingId = findOneWithKV [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]

findActiveByRBIdLite :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe RideLite)
findActiveByRBIdLite (Kernel.Types.Id.Id rbId) = findOneWithKV [Se.And [Se.Is Beam.bookingId $ Se.Eq rbId, Se.Is Beam.status $ Se.Not $ Se.Eq Domain.Types.Ride.CANCELLED]]

findInProgressByDriverId :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe RideLite)
findInProgressByDriverId (Kernel.Types.Id.Id driverId) = findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq driverId, Se.Is Beam.status $ Se.Eq Domain.Types.Ride.INPROGRESS]]

findRideByRideShortIdLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.ShortId RideLite -> m (Maybe RideLite))
findRideByRideShortIdLite shortId = findOneWithKV [Se.Is Beam.shortId $ Se.Eq (Kernel.Types.Id.getShortId shortId)]

findRidesFromDBLite :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Kernel.Types.Id.Id RideLite] -> m [RideLite]
findRidesFromDBLite rideIds = findAllWithDb [Se.Is Beam.id $ Se.In (Kernel.Types.Id.getId <$> rideIds)]

data RideLite = RideLite
  { bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    id :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    shortId :: Kernel.Types.Id.ShortId Domain.Types.Ride.Ride,
    status :: Domain.Types.Ride.RideStatus,
    driverArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    tripCategory :: Maybe Domain.Types.TripCategory
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

type RideLiteTable = Beam.RideT Identity

instance FromTType' RideLiteTable RideLite where
  fromTType' (Beam.RideT {..}) = do
    pure $
      Just
        RideLite
          { bookingId = Kernel.Types.Id.Id bookingId,
            id = Kernel.Types.Id.Id id,
            driverId = Kernel.Types.Id.Id driverId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            shortId = Kernel.Types.Id.ShortId shortId,
            status = status,
            driverArrivalTime = driverArrivalTime,
            tripCategory = tripCategory
          }
