{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.Tickets.TicketBookingService where

import Data.Aeson
import Domain.Types.Merchant.MerchantOperatingCity (MerchantOperatingCity)
import Domain.Types.Tickets.TicketBooking (TicketBooking)
import Domain.Types.Tickets.TicketService (TicketService)
import IssueManagement.Tools.UtilsTH (HighPrecMoney)
import Kernel.Prelude
import Kernel.Types.Id
import Tools.Beam.UtilsTH

data TicketBookingServicePriceBreakup = TicketBookingServicePriceBreakup
  { ticketBookingServiceId :: Id TicketBookingService,
    attendeeType :: Text,
    numberOfUnits :: Int,
    pricePerUnit :: HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data TicketBookingService = TicketBookingService
  { id :: Id TicketBookingService,
    shortId :: ShortId TicketBookingService,
    ticketBookingId :: Id TicketBooking,
    ticketServiceId :: Id TicketService,
    amount :: HighPrecMoney,
    status :: ServiceStatus,
    verificationCount :: Int,
    expiryDate :: Maybe UTCTime,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    prices :: [TicketBookingServicePriceBreakup],
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ServiceStatus = Pending | Failed | Confirmed | Verified
  deriving (Generic, Eq, Ord, Read, Show, FromJSON, ToJSON, ToSchema)

$(mkBeamInstancesForEnum ''ServiceStatus)
