{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FareBreakupInfo where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FareBreakup
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data FareBreakupInfoT f = FareBreakupInfoT
  { entityId :: (B.C f Kernel.Prelude.Text),
    entityType :: (B.C f Domain.Types.FareBreakup.FareBreakupEntityType),
    fareBreakups :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FareBreakupInfoT where
  data PrimaryKey FareBreakupInfoT f = FareBreakupInfoId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FareBreakupInfoId . id

type FareBreakupInfo = FareBreakupInfoT Identity

$(enableKVPG (''FareBreakupInfoT) [('id)] [[('entityId)]])

$(mkTableInstances (''FareBreakupInfoT) "fare_breakup_info")
