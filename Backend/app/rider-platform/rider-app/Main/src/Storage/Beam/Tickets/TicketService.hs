{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Tickets.TicketService where

import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

data TicketServiceT f = TicketServiceT
  { id :: B.C f Text,
    placesId :: B.C f Text,
    service :: B.C f Text,
    maxVerification :: B.C f Int,
    openTimings :: B.C f (Maybe TimeOfDay),
    closeTimings :: B.C f (Maybe TimeOfDay),
    validityTimings :: B.C f (Maybe TimeOfDay)
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketServiceT where
  data PrimaryKey TicketServiceT f = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type TicketService = TicketServiceT Identity

$(enableKVPG ''TicketServiceT ['id] [['placesId]])

$(mkTableInstances ''TicketServiceT "ticket_service")
