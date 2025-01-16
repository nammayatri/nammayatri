{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Beam.TimeBoundConfig where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.TimeBound
import qualified Lib.Yudhishthira.Types
import Tools.Beam.UtilsTH

data TimeBoundConfigT f = TimeBoundConfigT
  { merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    timeBoundDomain :: B.C f Lib.Yudhishthira.Types.LogicDomain,
    timeBounds :: B.C f Kernel.Types.TimeBound.TimeBound,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table TimeBoundConfigT where
  data PrimaryKey TimeBoundConfigT f = TimeBoundConfigId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Lib.Yudhishthira.Types.LogicDomain) deriving (Generic, B.Beamable)
  primaryKey = TimeBoundConfigId <$> merchantOperatingCityId <*> name <*> timeBoundDomain

type TimeBoundConfig = TimeBoundConfigT Identity

$(enableKVPG ''TimeBoundConfigT ['merchantOperatingCityId, 'name, 'timeBoundDomain] [])

$(mkTableInstancesGenericSchema ''TimeBoundConfigT "time_bound_config")
