{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RideDetails where

import qualified Database.Beam as B
import qualified Domain.Types.Vehicle
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data RideDetailsT f = RideDetailsT
  { createdAt :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    defaultServiceTierName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverCountryCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverName :: B.C f Kernel.Prelude.Text,
    driverNumberEncrypted :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    driverNumberHash :: B.C f (Kernel.Prelude.Maybe Kernel.External.Encryption.DbHash),
    fleetOwnerId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    id :: B.C f Kernel.Prelude.Text,
    vehicleClass :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleColor :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleModel :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    vehicleNumber :: B.C f Kernel.Prelude.Text,
    vehicleVariant :: B.C f (Kernel.Prelude.Maybe Domain.Types.Vehicle.Variant)
  }
  deriving (Generic, B.Beamable)

instance B.Table RideDetailsT where
  data PrimaryKey RideDetailsT f = RideDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RideDetailsId . id

type RideDetails = RideDetailsT Identity

$(enableKVPG ''RideDetailsT ['id] [])

$(mkTableInstances ''RideDetailsT "ride_details")

{-
	DSL Source Link: file://./../../../spec/Storage/RideDetails.yaml
-}
