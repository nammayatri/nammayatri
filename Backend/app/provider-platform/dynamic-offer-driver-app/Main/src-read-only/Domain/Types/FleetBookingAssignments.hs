{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FleetBookingAssignments where

import Data.Aeson
import qualified Data.Time
import qualified Domain.Types.FleetBookingInformation
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FleetBookingAssignments = FleetBookingAssignments
  { amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    assignmentEndTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    assignmentStartTime :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    bookingId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    fleetOwnerId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.FleetBookingAssignments.FleetBookingAssignments,
    mainAssignmentId :: Kernel.Types.Id.Id Domain.Types.FleetBookingInformation.FleetBookingInformation,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    paymentMethod :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    placeName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    serviceName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    skuDurationMins :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    vehicleNo :: Kernel.Prelude.Text,
    visitDate :: Kernel.Prelude.Maybe Data.Time.Day
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
