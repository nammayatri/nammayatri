{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TicketPlace where

import qualified Database.Beam as B
import qualified Domain.Types.TicketPlace
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TicketPlaceT f = TicketPlaceT
  { id :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    lat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    lon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    gallery :: B.C f [Kernel.Prelude.Text],
    shortDesc :: B.C f Kernel.Prelude.Text,
    iconUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    termsAndConditions :: B.C f [Kernel.Prelude.Text],
    placeType :: B.C f Domain.Types.TicketPlace.PlaceType,
    mapImageUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    openTimings :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay),
    closeTimings :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay),
    status :: B.C f Domain.Types.TicketPlace.PlaceStatus,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TicketPlaceT where
  data PrimaryKey TicketPlaceT f = TicketPlaceId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = TicketPlaceId . id

type TicketPlace = TicketPlaceT Identity

$(enableKVPG ''TicketPlaceT ['id] [])

$(mkTableInstances ''TicketPlaceT "ticket_place")
