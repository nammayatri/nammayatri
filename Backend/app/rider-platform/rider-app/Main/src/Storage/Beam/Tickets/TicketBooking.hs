{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Storage.Beam.Tickets.TicketBooking where

import Data.Time (Day)
import qualified Database.Beam as B
import Domain.Types.Tickets.TicketBooking (BookingStatus)
import Kernel.Prelude
import Kernel.Utils.Common (HighPrecMoney)
import Tools.Beam.UtilsTH

data TicketBookingT f = TicketBookingT
  { id :: B.C f Text,
    shortId :: B.C f Text,
    merchantOperatingCityId :: B.C f Text,
    ticketPlaceId :: B.C f Text,
    personId :: B.C f Text,
    amount :: B.C f HighPrecMoney,
    visitDate :: B.C f Day,
    status :: B.C f BookingStatus,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketBookingT where
  data PrimaryKey TicketBookingT f = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type TicketBooking = TicketBookingT Identity

$(enableKVPG ''TicketBookingT ['id] [])

$(mkTableInstances ''TicketBookingT "ticket_booking")
