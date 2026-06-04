{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.DriverIdentityInfo where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DriverInformation
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Tools.Beam.UtilsTH

data DriverIdentityInfoT f = DriverIdentityInfoT
  { address :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    addressDocumentType :: B.C f (Kernel.Prelude.Maybe Domain.Types.DriverInformation.AddressDocumentType),
    addressState :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Beckn.Context.IndianState),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    driverId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    nomineeDob :: B.C f (Kernel.Prelude.Maybe Data.Time.Day),
    nomineeName :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    nomineeRelationship :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverIdentityInfoT where
  data PrimaryKey DriverIdentityInfoT f = DriverIdentityInfoId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DriverIdentityInfoId . driverId

type DriverIdentityInfo = DriverIdentityInfoT Identity

$(enableKVPG ''DriverIdentityInfoT ['driverId] [])

$(mkTableInstances ''DriverIdentityInfoT "driver_identity_info")
