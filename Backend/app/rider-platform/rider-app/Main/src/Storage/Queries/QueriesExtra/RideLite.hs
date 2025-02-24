{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.QueriesExtra.RideLite where

import qualified Domain.Types.Booking
import qualified Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Ride
import Kernel.Beam.Functions
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.Ride as Beam
import qualified Storage.Queries.Extra.Transformers.Ride

---------------- use this function if you need the data which are here as per your domain requirement ----------------

findByIdLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id RideLite -> m (Maybe RideLite))
findByIdLite id = findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByRBIdLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe RideLite))
findByRBIdLite bookingId = findOneWithKV [Se.Is Beam.bookingId $ Se.Eq (Kernel.Types.Id.getId bookingId)]

findActiveByRBIdLite :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Booking.Booking -> m (Maybe RideLite)
findActiveByRBIdLite (Kernel.Types.Id.Id rbId) = findOneWithKV [Se.And [Se.Is Beam.bookingId $ Se.Eq rbId, Se.Is Beam.status $ Se.Not $ Se.Eq Domain.Types.Ride.CANCELLED]]

findRideByRideShortIdLite :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.ShortId RideLite -> m (Maybe RideLite))
findRideByRideShortIdLite shortId = findOneWithKV [Se.Is Beam.shortId $ Se.Eq (Kernel.Types.Id.getShortId shortId)]

data RideLite = RideLite
  { bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    driverName :: Kernel.Prelude.Text,
    driverRating :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    estimatedEndTimeRange :: Kernel.Prelude.Maybe Domain.Types.Ride.EstimatedEndTimeRange,
    id :: Kernel.Types.Id.Id Domain.Types.Ride.Ride,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    rideRating :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.Ride.Ride,
    talkedWithDriver :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    status :: Domain.Types.Ride.RideStatus,
    driverArrivalTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    destinationReachedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    driversPreviousRideDropLoc :: Kernel.Prelude.Maybe Kernel.External.Maps.LatLong,
    showDriversPreviousRideDropLoc :: Kernel.Prelude.Bool
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

type RideLiteTable = Beam.RideT Identity

instance FromTType' RideLiteTable RideLite where
  fromTType' (Beam.RideT {..}) = do
    pure $
      Just
        RideLite
          { bookingId = Kernel.Types.Id.Id bookingId,
            estimatedEndTimeRange = Storage.Queries.Extra.Transformers.Ride.mkEstimatedEndTimeRange <$> estimatedEndTimeRangeStart <*> estimatedEndTimeRangeEnd,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            shortId = Kernel.Types.Id.ShortId shortId,
            driversPreviousRideDropLoc = Storage.Queries.Extra.Transformers.Ride.mkLatLong driversPreviousRideDropLat driversPreviousRideDropLon,
            showDriversPreviousRideDropLoc = Kernel.Prelude.fromMaybe False showDriversPreviousRideDropLoc,
            ..
          }
