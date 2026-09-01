{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FRFSPassengerDetail where

import Data.Aeson
import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSTicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Seat
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FRFSPassengerDetail = FRFSPassengerDetail
  { age :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bookingId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.FRFSTicketBooking.FRFSTicketBooking),
    dropOffPointPlaceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    gender :: Domain.Types.Person.Gender,
    id :: Kernel.Types.Id.Id Domain.Types.FRFSPassengerDetail.FRFSPassengerDetail,
    isChild :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    name :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    pickupPointPlaceId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    quoteId :: Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote,
    seatId :: Kernel.Types.Id.Id Domain.Types.Seat.Seat,
    seatLabel :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)
