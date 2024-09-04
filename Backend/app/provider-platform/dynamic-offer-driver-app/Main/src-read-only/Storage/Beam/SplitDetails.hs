{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SplitDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Plan
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SplitDetailsT f = SplitDetailsT
  { amountPercentage :: B.C f Kernel.Prelude.Double,
    fixedAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    id :: B.C f Kernel.Prelude.Text,
    serviceName :: B.C f Domain.Types.Plan.ServiceNames,
    vendorId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SplitDetailsT where
  data PrimaryKey SplitDetailsT f = SplitDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SplitDetailsId . id

type SplitDetails = SplitDetailsT Identity

$(enableKVPG ''SplitDetailsT ['id] [])

$(mkTableInstances ''SplitDetailsT "split_details")
