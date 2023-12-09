{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TicketBookingService where

import qualified Database.Beam as B
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketBookingService
import qualified Domain.Types.TicketService
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data TicketBookingServiceT f = TicketBookingServiceT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    btype :: B.C f Domain.Types.BusinessHour.BusinessHourType,
    expiryDate :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    id :: B.C f Kernel.Prelude.Text,
    shortId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.TicketBookingService.ServiceStatus,
    ticketBookingId :: B.C f Kernel.Prelude.Text,
    ticketServiceId :: B.C f Kernel.Prelude.Text,
    verificationCount :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketBookingServiceT where
  data PrimaryKey TicketBookingServiceT f = TicketBookingServiceId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = TicketBookingServiceId . id

type TicketBookingService = TicketBookingServiceT Identity

$(enableKVPG ''TicketBookingServiceT ['id] [])

$(mkTableInstances ''TicketBookingServiceT "ticket_booking_service")
