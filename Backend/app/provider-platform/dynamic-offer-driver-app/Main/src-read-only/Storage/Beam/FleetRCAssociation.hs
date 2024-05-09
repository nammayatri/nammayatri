{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FleetRCAssociation where

import qualified Database.Beam as B
import qualified Domain.Types.IdfyVerification
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FleetRCAssociationT f = FleetRCAssociationT
  { associatedOn :: (B.C f Kernel.Prelude.UTCTime),
    associatedTill :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime)),
    driverId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    isRcActive :: (B.C f Kernel.Prelude.Bool),
    rcId :: (B.C f Kernel.Prelude.Text),
    rcVerificationStatus :: (B.C f Domain.Types.IdfyVerification.VerificationStatus),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FleetRCAssociationT where
  data PrimaryKey FleetRCAssociationT f = FleetRCAssociationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FleetRCAssociationId . id

type FleetRCAssociation = FleetRCAssociationT Identity

$(enableKVPG (''FleetRCAssociationT) [('id)] [[('driverId)], [('rcId)]])

$(mkTableInstances (''FleetRCAssociationT) "fleet_rc_association")
