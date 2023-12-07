{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TicketBookingServiceCategory where

import qualified Database.Beam as B
import qualified Domain.Types.TicketBookingService
import qualified Domain.Types.TicketBookingServiceCategory
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data TicketBookingServiceCategoryT f = TicketBookingServiceCategoryT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    bookedSeats :: B.C f Kernel.Prelude.Int,
    id :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    serviceCategoryId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    ticketBookingServiceId :: B.C f Kernel.Prelude.Text
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketBookingServiceCategoryT where
  data PrimaryKey TicketBookingServiceCategoryT f = TicketBookingServiceCategoryId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = TicketBookingServiceCategoryId . id

type TicketBookingServiceCategory = TicketBookingServiceCategoryT Identity

$(enableKVPG ''TicketBookingServiceCategoryT ['id] [])

$(mkTableInstances ''TicketBookingServiceCategoryT "ticket_booking_service_category")
