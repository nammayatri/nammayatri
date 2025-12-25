{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TicketSubPlace where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.TicketSubPlace
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TicketSubPlaceT f = TicketSubPlaceT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    enforcedTicketPlaceId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    isActive :: B.C f Kernel.Prelude.Bool,
    name :: B.C f Kernel.Prelude.Text,
    rules :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    subPlaceType :: B.C f Domain.Types.TicketSubPlace.SubPlaceType,
    ticketPlaceId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketSubPlaceT where
  data PrimaryKey TicketSubPlaceT f = TicketSubPlaceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TicketSubPlaceId . id

type TicketSubPlace = TicketSubPlaceT Identity

$(enableKVPG ''TicketSubPlaceT ['id] [['ticketPlaceId]])

$(mkTableInstances ''TicketSubPlaceT "ticket_sub_place")
