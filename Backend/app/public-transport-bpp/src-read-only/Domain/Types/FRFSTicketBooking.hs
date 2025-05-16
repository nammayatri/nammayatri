{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSTicketBooking where

import Data.Aeson
import qualified Domain.Types.FRFSSearchRequest
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.StationFare
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSTicketBooking = FRFSTicketBooking
  { bapId :: Kernel.Prelude.Text,
    bookingType :: Domain.Types.FRFSTicketBooking.BookingTypeEnum,
    bppId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking,
    quantity :: Kernel.Prelude.Int,
    selectedFareId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.StationFare.StationFare),
    status :: Domain.Types.FRFSTicketBooking.BookingStatusEnum,
    transactionId :: Kernel.Types.Id.Id Domain.Types.FRFSSearchRequest.FRFSSearchRequest,
    updatedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity)
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BookingStatusEnum = CREATED | CONFIRMED | CANCELLED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data BookingTypeEnum = SINGLE_JOURNEY | ROUND_TRIP deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''BookingStatusEnum))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''BookingTypeEnum))
