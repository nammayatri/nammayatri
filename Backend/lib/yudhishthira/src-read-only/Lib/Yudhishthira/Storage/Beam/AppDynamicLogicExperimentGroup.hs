{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Beam.AppDynamicLogicExperimentGroup where

import qualified Data.Text
import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import Tools.Beam.UtilsTH

data AppDynamicLogicExperimentGroupT f = AppDynamicLogicExperimentGroupT
  { experimentStatus :: (B.C f (Kernel.Prelude.Maybe Lib.Yudhishthira.Types.ExperimentStatus)),
    groupName :: (B.C f Data.Text.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    merchantOperatingCityId :: (B.C f Data.Text.Text),
    modifiedBy :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    rolloutPercentage :: (B.C f Kernel.Prelude.Int),
    timeBounds :: (B.C f Data.Text.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table AppDynamicLogicExperimentGroupT where
  data PrimaryKey AppDynamicLogicExperimentGroupT f = AppDynamicLogicExperimentGroupId (B.C f Data.Text.Text) (B.C f Data.Text.Text) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = AppDynamicLogicExperimentGroupId <$> groupName <*> merchantOperatingCityId <*> timeBounds

type AppDynamicLogicExperimentGroup = AppDynamicLogicExperimentGroupT Identity

$(enableKVPG (''AppDynamicLogicExperimentGroupT) [('groupName), ('merchantOperatingCityId), ('timeBounds)] [])

$(mkTableInstancesGenericSchema (''AppDynamicLogicExperimentGroupT) "app_dynamic_logic_experiment_group")
