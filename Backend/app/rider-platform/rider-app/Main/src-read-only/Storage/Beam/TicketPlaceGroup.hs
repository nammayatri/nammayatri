{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TicketPlaceGroup where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TicketPlaceGroupT f = TicketPlaceGroupT
  { description :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    geomGeoJson :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    isActive :: (B.C f Kernel.Prelude.Bool),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    name :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketPlaceGroupT where
  data PrimaryKey TicketPlaceGroupT f = TicketPlaceGroupId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TicketPlaceGroupId . id

type TicketPlaceGroup = TicketPlaceGroupT Identity

$(enableKVPG (''TicketPlaceGroupT) [('id)] [])

$(mkTableInstances (''TicketPlaceGroupT) "ticket_place_group")
