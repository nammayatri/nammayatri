{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.TripTransaction where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.TripTransaction
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data TripTransactionT f = TripTransactionT
  { allowEndingMidRoute :: B.C f Kernel.Prelude.Bool,
    deviationCount :: B.C f Kernel.Prelude.Int,
    driverId :: B.C f Data.Text.Text,
    endLocationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    endLocationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    endStopCode :: B.C f Data.Text.Text,
    fleetOwnerId :: B.C f Data.Text.Text,
    id :: B.C f Data.Text.Text,
    isCurrentlyDeviated :: B.C f Kernel.Prelude.Bool,
    merchantId :: B.C f Data.Text.Text,
    merchantOperatingCityId :: B.C f Data.Text.Text,
    routeCode :: B.C f Data.Text.Text,
    sequenceNumber :: B.C f Kernel.Prelude.Int,
    startLocationLat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    startLocationLon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    startedNearStopCode :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    status :: B.C f Domain.Types.TripTransaction.TripStatus,
    tripCode :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    vehicleNumber :: B.C f Data.Text.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TripTransactionT where
  data PrimaryKey TripTransactionT f = TripTransactionId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = TripTransactionId . id

type TripTransaction = TripTransactionT Identity

$(enableKVPG ''TripTransactionT ['id] [])

$(mkTableInstances ''TripTransactionT "trip_transaction")