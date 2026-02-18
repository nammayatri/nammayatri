{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverUdyam where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DriverPanCard
import Kernel.External.Encryption
import qualified Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Documents
import Tools.Beam.UtilsTH

data DriverUdyamT f = DriverUdyamT
  { driverId :: (B.C f Kernel.Prelude.Text),
    enterpriseName :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    enterpriseType :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    id :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    udyamNumberEncrypted :: (B.C f Kernel.Prelude.Text),
    udyamNumberHash :: (B.C f Kernel.External.Encryption.DbHash),
    verificationStatus :: (B.C f Kernel.Types.Documents.VerificationStatus),
    verifiedBy :: (B.C f (Kernel.Prelude.Maybe Domain.Types.DriverPanCard.VerifiedBy)),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverUdyamT where
  data PrimaryKey DriverUdyamT f = DriverUdyamId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverUdyamId . id

type DriverUdyam = DriverUdyamT Identity

$(enableKVPG (''DriverUdyamT) [('id)] [[('driverId)]])

$(mkTableInstances (''DriverUdyamT) "driver_udyam")
