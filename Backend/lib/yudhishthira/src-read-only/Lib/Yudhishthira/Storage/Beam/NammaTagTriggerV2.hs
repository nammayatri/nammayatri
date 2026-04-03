{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Lib.Yudhishthira.Storage.Beam.NammaTagTriggerV2 where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import qualified Kernel.Prelude
import qualified Lib.Yudhishthira.Types
import qualified Database.Beam as B



data NammaTagTriggerV2T f
    = NammaTagTriggerV2T {createdAt :: (B.C f Kernel.Prelude.UTCTime),
                          event :: (B.C f Lib.Yudhishthira.Types.ApplicationEvent),
                          merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                          tagName :: (B.C f Kernel.Prelude.Text),
                          updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table NammaTagTriggerV2T
    where data PrimaryKey NammaTagTriggerV2T f = NammaTagTriggerV2Id (B.C f Lib.Yudhishthira.Types.ApplicationEvent) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = NammaTagTriggerV2Id <$> event <*> merchantOperatingCityId <*> tagName
type NammaTagTriggerV2 = NammaTagTriggerV2T Identity

$(enableKVPG (''NammaTagTriggerV2T) [('event), ('merchantOperatingCityId), ('tagName)] [])

$(mkTableInstancesGenericSchema (''NammaTagTriggerV2T) "namma_tag_trigger_v2")

