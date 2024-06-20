{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TicketBookingService where

import qualified Data.Time.Calendar
import qualified Database.Beam as B
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.TicketBookingService
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data TicketBookingServiceT f = TicketBookingServiceT
  { id :: B.C f Kernel.Prelude.Text,
    shortId :: B.C f Kernel.Prelude.Text,
    ticketBookingId :: B.C f Kernel.Prelude.Text,
    ticketServiceId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency),
    status :: B.C f Domain.Types.TicketBookingService.ServiceStatus,
    verificationCount :: B.C f Kernel.Prelude.Int,
    visitDate :: B.C f (Kernel.Prelude.Maybe Data.Time.Calendar.Day),
    btype :: B.C f Domain.Types.BusinessHour.BusinessHourType,
    bHourId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    expiryDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    bookedSeats :: B.C f Kernel.Prelude.Int,
    cancelledSeats :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketBookingServiceT where
  data PrimaryKey TicketBookingServiceT f = TicketBookingServiceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TicketBookingServiceId . id

type TicketBookingService = TicketBookingServiceT Identity

$(enableKVPG ''TicketBookingServiceT ['id] [])

$(mkTableInstances ''TicketBookingServiceT "ticket_booking_service")
