{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.BookingCancellationReason where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Booking
import qualified Kernel.Types.Common
import qualified Kernel.External.Maps
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.CancellationReason
import qualified Domain.Types.Ride
import qualified Tools.Beam.UtilsTH



data BookingCancellationReason
    = BookingCancellationReason {additionalInfo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                                 bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
                                 distanceUnit :: Kernel.Types.Common.DistanceUnit,
                                 driverCancellationLocation :: Kernel.Prelude.Maybe Kernel.External.Maps.LatLong,
                                 driverDistToPickup :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
                                 driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
                                 merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
                                 merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
                                 reasonCode :: Kernel.Prelude.Maybe Domain.Types.CancellationReason.CancellationReasonCode,
                                 rideId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Ride.Ride),
                                 source :: Domain.Types.BookingCancellationReason.CancellationSource}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
data CancellationSource = ByUser | ByDriver | ByMerchant | ByAllocator | ByApplication | ByFleetOwner deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''CancellationSource))

