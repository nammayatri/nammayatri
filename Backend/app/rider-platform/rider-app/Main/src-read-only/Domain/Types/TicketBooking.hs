{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.TicketBooking where

import Data.Aeson
import qualified Data.Time.Calendar
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.TicketPlace
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import Tools.Beam.UtilsTH

data TicketBooking = TicketBooking
  { amount :: Kernel.Types.Common.HighPrecMoney,
    id :: Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking,
    status :: Domain.Types.TicketBooking.BookingStatus,
    ticketPlaceId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    visitDate :: Data.Time.Calendar.Day,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data BookingStatus = Pending | Failed | Booked
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''BookingStatus)

$(mkHttpInstancesForEnum ''BookingStatus)
