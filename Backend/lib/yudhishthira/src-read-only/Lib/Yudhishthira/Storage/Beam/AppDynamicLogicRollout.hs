{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Yudhishthira.Storage.Beam.AppDynamicLogicRollout where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Lib.Yudhishthira.Types
import qualified Kernel.Prelude
import qualified Data.Text
import qualified Database.Beam as B



data AppDynamicLogicRolloutT f
    = AppDynamicLogicRolloutT {domain :: (B.C f Lib.Yudhishthira.Types.LogicDomain),
                               experimentStatus :: (B.C f (Kernel.Prelude.Maybe Lib.Yudhishthira.Types.ExperimentStatus)),
                               isBaseVersion :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                               merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
                               merchantOperatingCityId :: (B.C f Data.Text.Text),
                               modifiedBy :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
                               percentageRollout :: (B.C f Kernel.Prelude.Int),
                               timeBounds :: (B.C f Data.Text.Text),
                               version :: (B.C f Kernel.Prelude.Int),
                               versionDescription :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
                               createdAt :: (B.C f Kernel.Prelude.UTCTime),
                               updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table AppDynamicLogicRolloutT
    where data PrimaryKey AppDynamicLogicRolloutT f
              = AppDynamicLogicRolloutId (B.C f Lib.Yudhishthira.Types.LogicDomain) (B.C f Data.Text.Text) (B.C f Data.Text.Text) (B.C f Kernel.Prelude.Int)
              deriving (Generic, B.Beamable)
          primaryKey = AppDynamicLogicRolloutId <$> domain <*> merchantOperatingCityId <*> timeBounds <*> version
type AppDynamicLogicRollout = AppDynamicLogicRolloutT Identity

$(enableKVPG (''AppDynamicLogicRolloutT) [('domain), ('merchantOperatingCityId), ('timeBounds), ('version)] [])

$(mkTableInstancesGenericSchema (''AppDynamicLogicRolloutT) "app_dynamic_logic_rollout")

