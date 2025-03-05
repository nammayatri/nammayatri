{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.TicketBooking (module Domain.Types.TicketBooking, module ReExport) where

import Data.Aeson
import qualified Data.Time
import Domain.Types.Extra.TicketBooking as ReExport
import qualified Domain.Types.Extra.TicketBooking
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.TicketPlace
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH
import qualified Tools.Payment

data TicketBooking = TicketBooking
  { amount :: Kernel.Types.Common.Price,
    blockExpirationTime :: Kernel.Prelude.Maybe Kernel.Prelude.Double,
    bookedSeats :: Kernel.Prelude.Int,
    cancelledSeats :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking,
    status :: Domain.Types.Extra.TicketBooking.BookingStatus,
    ticketPlaceId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    updatedAt :: Kernel.Prelude.UTCTime,
    vendorSplitDetails :: Kernel.Prelude.Maybe [Tools.Payment.VendorSplitDetails],
    visitDate :: Data.Time.Day,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant)
  }
  deriving (Generic, Show)
