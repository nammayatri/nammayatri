{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SharedBooking where

import Data.Aeson
import qualified Domain.Types.Booking
import qualified Domain.Types.Location
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.ServiceTierType
import qualified Domain.Types.SharedEstimate
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Tools.Beam.UtilsTH

data SharedBooking = SharedBooking
  { bookingIds :: [Kernel.Types.Id.Id Domain.Types.Booking.Booking],
    createdAt :: Kernel.Prelude.UTCTime,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    driverId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    estimatedDistance :: Kernel.Prelude.Maybe Kernel.Types.Common.Meters,
    estimatedDuration :: Kernel.Prelude.Maybe Kernel.Types.Common.Seconds,
    estimatedTotalFare :: Kernel.Types.Common.HighPrecMoney,
    fromLocationIds :: [Kernel.Types.Id.Id Domain.Types.Location.Location],
    id :: Kernel.Types.Id.Id Domain.Types.SharedBooking.SharedBooking,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    pairingTime :: Kernel.Prelude.UTCTime,
    providerId :: Kernel.Prelude.Text,
    providerUrl :: Kernel.Types.Common.BaseUrl,
    sharedEstimateId :: Kernel.Types.Id.Id Domain.Types.SharedEstimate.SharedEstimate,
    status :: Domain.Types.SharedBooking.BookingStatus,
    toLocationIds :: [Kernel.Types.Id.Id Domain.Types.Location.Location],
    tollNames :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    transactionId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleServiceTier :: Domain.Types.ServiceTierType.ServiceTierType
  }
  deriving (Generic, Show)

data BookingStatus = NEW | TRIP_ASSIGNED | COMPLETED | CANCELLED | REALLOCATED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''BookingStatus))

$(mkHttpInstancesForEnum (''BookingStatus))
