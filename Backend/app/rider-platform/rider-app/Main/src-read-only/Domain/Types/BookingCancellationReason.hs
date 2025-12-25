{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BookingCancellationReason where

import Data.Aeson
import qualified Domain.Types.Booking
import qualified Domain.Types.CancellationReason
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Kernel.External.Maps
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data BookingCancellationReason = BookingCancellationReason
  { additionalInfo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
    createdAt :: Kernel.Prelude.UTCTime,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverCancellationLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.LatLong,
    driverDistToPickup :: Kernel.Prelude.Maybe Kernel.Types.Common.Distance,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    reasonCode :: Kernel.Prelude.Maybe Domain.Types.CancellationReason.CancellationReasonCode,
    reasonStage :: Kernel.Prelude.Maybe Domain.Types.CancellationReason.CancellationStage,
    rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride),
    riderId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    source :: Domain.Types.BookingCancellationReason.CancellationSource,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data CancellationSource = ByUser | ByDriver | ByMerchant | ByAllocator | ByApplication deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''CancellationSource)
