{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TicketService where

import qualified Database.Beam as B
import qualified Domain.Types.BusinessHour as Domain.Types.BusinessHour
import qualified Domain.Types.TicketService as Domain.Types.TicketService
import Kernel.Prelude
import qualified Kernel.Prelude as Kernel.Prelude
import qualified Kernel.Types.Id as Kernel.Types.Id
import Tools.Beam.UtilsTH

data TicketServiceT f = TicketServiceT
  { allowFutureBooking :: B.C f Kernel.Prelude.Bool,
    businessHours :: B.C f [Kernel.Prelude.Text],
    expiry :: B.C f Domain.Types.TicketService.ExpiryType,
    id :: B.C f Kernel.Prelude.Text,
    maxVerification :: B.C f Kernel.Prelude.Int,
    operationalDays :: B.C f [Kernel.Prelude.Text],
    placeId :: B.C f Kernel.Prelude.Text,
    serviceName :: B.C f Kernel.Prelude.Text,
    shortDesc :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketServiceT where
  data PrimaryKey TicketServiceT f = TicketServiceId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = TicketServiceId . id

type TicketService = TicketServiceT Identity

$(enableKVPG ''TicketServiceT ['id] [])

$(mkTableInstances ''TicketServiceT "ticket_service")
