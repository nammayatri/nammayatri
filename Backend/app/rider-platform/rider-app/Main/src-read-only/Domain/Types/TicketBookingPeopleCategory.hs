{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.TicketBookingPeopleCategory where

import qualified Domain.Types.TicketBookingServiceCategory as Domain.Types.TicketBookingServiceCategory
import Kernel.Prelude
import qualified Kernel.Types.Common as Kernel.Types.Common
import qualified Kernel.Types.Id as Kernel.Types.Id

data TicketBookingPeopleCategory = TicketBookingPeopleCategory
  { id :: Kernel.Types.Id.Id Domain.Types.TicketBookingPeopleCategory.TicketBookingPeopleCategory,
    name :: Kernel.Prelude.Text,
    numberOfUnits :: Kernel.Prelude.Int,
    pricePerUnit :: Kernel.Types.Common.HighPrecMoney,
    ticketBookingServiceCategoryId :: Kernel.Types.Id.Id Domain.Types.TicketBookingServiceCategory.TicketBookingServiceCategory
  }
  deriving (Generic, Show)
