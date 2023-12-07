{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.TicketBookingServiceCategory where

import qualified Domain.Types.TicketBookingService
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id

data TicketBookingServiceCategory = TicketBookingServiceCategory
  { amount :: Kernel.Types.Common.HighPrecMoney,
    bookedSeats :: Kernel.Prelude.Int,
    id :: Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory,
    name :: Kernel.Prelude.Text,
    serviceCategoryId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ticketBookingServiceId :: Kernel.Types.Id.Id Domain.Types.TicketBookingService.TicketBookingService
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
