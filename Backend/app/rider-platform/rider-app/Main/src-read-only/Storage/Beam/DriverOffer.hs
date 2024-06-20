{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverOffer where

import qualified Database.Beam as B
import qualified Domain.Types.DriverOffer
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data DriverOfferT f = DriverOfferT
  { id :: B.C f Kernel.Prelude.Text,
    estimateId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverName :: B.C f Kernel.Prelude.Text,
    durationToPickup :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    distanceToPickup :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMeters),
    distanceToPickupValue :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecDistance),
    distanceUnit :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.DistanceUnit),
    validTill :: B.C f Kernel.Prelude.UTCTime,
    bppQuoteId :: B.C f Kernel.Prelude.Text,
    rating :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal),
    status :: B.C f Domain.Types.DriverOffer.DriverOfferStatus,
    createdAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverOfferT where
  data PrimaryKey DriverOfferT f = DriverOfferId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverOfferId . id

type DriverOffer = DriverOfferT Identity

$(enableKVPG ''DriverOfferT ['id] [['estimateId], ['bppQuoteId]])

$(mkTableInstances ''DriverOfferT "driver_offer")
