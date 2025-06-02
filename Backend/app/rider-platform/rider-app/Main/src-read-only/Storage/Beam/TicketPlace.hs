{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TicketPlace where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.TicketPlace
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TicketPlaceT f = TicketPlaceT
  { allowSameDayBooking :: B.C f Kernel.Prelude.Bool,
    closeTimings :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay),
    customTabs :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    gallery :: B.C f [Kernel.Prelude.Text],
    iconUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    lat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    lon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    mapImageUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    openTimings :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.TimeOfDay),
    placeType :: B.C f Domain.Types.TicketPlace.PlaceType,
    priority :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    recommend :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    rules :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    shortDesc :: B.C f Kernel.Prelude.Text,
    status :: B.C f Domain.Types.TicketPlace.PlaceStatus,
    termsAndConditions :: B.C f [Kernel.Prelude.Text],
    termsAndConditionsUrl :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    ticketMerchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
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
