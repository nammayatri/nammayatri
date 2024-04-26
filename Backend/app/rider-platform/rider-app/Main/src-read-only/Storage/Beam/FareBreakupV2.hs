{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FareBreakupV2 where

import qualified Database.Beam as B
import qualified Domain.Types.FareBreakupV2
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FareBreakupV2T f = FareBreakupV2T
  { amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    description :: (B.C f Kernel.Prelude.Text),
    entityId :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    tag :: (B.C f Domain.Types.FareBreakupV2.FareBreakupV2Tags),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)))
  }
  deriving (Generic, B.Beamable)

instance B.Table FareBreakupV2T where
  data PrimaryKey FareBreakupV2T f = FareBreakupV2Id (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FareBreakupV2Id . id

type FareBreakupV2 = FareBreakupV2T Identity

$(enableKVPG (''FareBreakupV2T) [('id)] [[('entityId)]])

$(mkTableInstances (''FareBreakupV2T) "fare_breakup_v2")
