{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetBookingInformation where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FleetBookingInformation = FleetBookingInformation
  { amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    bookedSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bookingId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    fleetOwnerId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    id :: Kernel.Types.Id.Id Domain.Types.FleetBookingInformation.FleetBookingInformation,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    personId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    placeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketPlaceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleNo :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    visitDate :: Kernel.Prelude.Maybe Data.Time.Day
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
