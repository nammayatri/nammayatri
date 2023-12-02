{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.TicketBooking where

import qualified Data.Time.Calendar as Data.Time.Calendar
import qualified Domain.Types.Merchant.MerchantOperatingCity as Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person as Domain.Types.Person
import qualified Domain.Types.TicketPlace as Domain.Types.TicketPlace
import Kernel.Prelude
import qualified Kernel.Types.Common as Kernel.Types.Common
import qualified Kernel.Types.Id as Kernel.Types.Id
import Tools.Beam.UtilsTH

data TicketBooking = TicketBooking
  { amount :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.TicketBooking.TicketBooking,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.Merchant.MerchantOperatingCity.MerchantOperatingCity,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    shortId :: Kernel.Types.Id.ShortId Domain.Types.TicketBooking.TicketBooking,
    status :: Domain.Types.TicketBooking.BookingStatus,
    ticketPlaceId :: Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace,
    updatedAt :: Kernel.Prelude.UTCTime,
    visitDate :: Data.Time.Calendar.Day
  }
  deriving (Generic, Show)

data BookingStatus = Pending | Confirmed | Cancelled | Expired
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForEnum ''BookingStatus)
