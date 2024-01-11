{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TicketBooking where

import qualified Data.Time.Calendar
import qualified Database.Beam as B
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.TicketBooking
import qualified Domain.Types.TicketPlace
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data TicketBookingT f = TicketBookingT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    personId :: B.C f Kernel.Prelude.Text,
    shortId :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.TicketBooking.BookingStatus,
    ticketPlaceId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    visitDate :: B.C f Data.Time.Calendar.Day,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketBookingT where
  data PrimaryKey TicketBookingT f = TicketBookingId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = TicketBookingId . id

type TicketBooking = TicketBookingT Identity

$(enableKVPG ''TicketBookingT ['id] [['personId, 'shortId]])

$(mkTableInstances ''TicketBookingT "ticket_booking")
