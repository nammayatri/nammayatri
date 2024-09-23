{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSSearch where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Station
import Kernel.External.Encryption
import Kernel.Prelude hiding (sequence)
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FRFSSearchT f = FRFSSearchT
  { fromStationId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    agency :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    convenienceCost :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    journeyId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    journeyLegOrder :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    skipBooking :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    partnerOrgId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    partnerOrgTransactionId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    quantity :: (B.C f Kernel.Prelude.Int),
    riderId :: (B.C f Kernel.Prelude.Text),
    toStationId :: (B.C f Kernel.Prelude.Text),
    vehicleType :: (B.C f Domain.Types.Station.FRFSVehicleType),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSSearchT where
  data PrimaryKey FRFSSearchT f = FRFSSearchId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSSearchId . id

type FRFSSearch = FRFSSearchT Identity

$(enableKVPG (''FRFSSearchT) [('id)] [[('riderId)]])

$(mkTableInstances (''FRFSSearchT) "frfs_search")
