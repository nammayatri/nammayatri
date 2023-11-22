{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Tickets.TicketPlace where

import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

data TicketPlaceT f = TicketPlaceT
  { id :: B.C f Text,
    merchantOperatingCityId :: B.C f Text,
    name :: B.C f Text,
    description :: B.C f (Maybe Text),
    lat :: B.C f (Maybe Double),
    lon :: B.C f (Maybe Double),
    gallery :: B.C f [Text],
    openTimings :: B.C f (Maybe TimeOfDay),
    closeTimings :: B.C f (Maybe TimeOfDay)
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketPlaceT where
  data PrimaryKey TicketPlaceT f = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type TicketPlace = TicketPlaceT Identity

$(enableKVPG ''TicketPlaceT ['id] [['merchantOperatingCityId]])

$(mkTableInstances ''TicketPlaceT "ticket_place")
