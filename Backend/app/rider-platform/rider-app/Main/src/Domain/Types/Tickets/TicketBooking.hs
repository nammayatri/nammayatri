{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Tickets.TicketBooking where

import Beckn.Types.Core.Taxi.Search (Person)
import Data.Aeson
import Data.Time (Day)
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Tickets.TicketPlace (TicketPlace)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id (Id, ShortId)
import Kernel.Utils.TH (mkHttpInstancesForEnum)
import Tools.Beam.UtilsTH

data TicketBooking = TicketBooking
  { id :: Id TicketBooking,
    shortId :: ShortId TicketBooking,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    ticketPlaceId :: Id TicketPlace,
    personId :: Id Person,
    amount :: HighPrecMoney,
    visitDate :: Day,
    status :: BookingStatus,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show)

data BookingStatus = Pending | Failed | Booked
  deriving (Generic, Read, Eq, Ord, Show, FromJSON, ToJSON, ToSchema, ToParamSchema)

$(mkBeamInstancesForEnum ''BookingStatus)
$(mkHttpInstancesForEnum ''BookingStatus)
