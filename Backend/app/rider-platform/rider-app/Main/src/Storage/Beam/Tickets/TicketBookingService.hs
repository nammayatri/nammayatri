{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Storage.Beam.Tickets.TicketBookingService where

import qualified Database.Beam as B
import qualified Domain.Types.Tickets.TicketBookingService as DTB
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Tools.Beam.UtilsTH

data TicketBookingServiceT f = TicketBookingServiceT
  { id :: B.C f Text,
    shortId :: B.C f Text,
    ticketBookingId :: B.C f Text,
    ticketServiceId :: B.C f Text,
    amount :: B.C f HighPrecMoney,
    status :: B.C f DTB.ServiceStatus,
    verificationCount :: B.C f Int,
    expiryDate :: B.C f (Maybe UTCTime),
    merchantOperatingCityId :: B.C f Text,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketBookingServiceT where
  data PrimaryKey TicketBookingServiceT f = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type TicketBookingService = TicketBookingServiceT Identity

$(enableKVPG ''TicketBookingServiceT ['id] [['ticketBookingId], ['shortId]])

$(mkTableInstances ''TicketBookingServiceT "ticket_booking_service")
