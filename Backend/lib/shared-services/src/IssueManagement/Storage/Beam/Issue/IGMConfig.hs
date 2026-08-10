{-# LANGUAGE StandaloneDeriving #-}

module IssueManagement.Storage.Beam.Issue.IGMConfig where

import qualified Database.Beam as B
import Database.Beam.MySQL ()
import IssueManagement.Tools.UtilsTH hiding (label)

data IGMConfigT f = IGMConfigT
  { expectedResolutionTime :: B.C f Int,
    expectedResponseTime :: B.C f Int,
    groEmail :: B.C f Text,
    groName :: B.C f Text,
    groPhone :: B.C f Text,
    respondentName :: B.C f (Maybe Text),
    respondentPhone :: B.C f (Maybe Text),
    respondentEmail :: B.C f (Maybe Text),
    resolutionProviderName :: B.C f (Maybe Text),
    resolutionProviderPhone :: B.C f (Maybe Text),
    resolutionProviderEmail :: B.C f (Maybe Text),
    id :: B.C f Text,
    merchantId :: B.C f Text,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IGMConfigT where
  data PrimaryKey IGMConfigT f = IGMConfigId (B.C f Text) deriving (Generic, B.Beamable)
  primaryKey = IGMConfigId . id

type IGMConfig = IGMConfigT Identity

$(enableKVPG ''IGMConfigT ['id] [['merchantId]])

$(mkTableInstancesGenericSchema ''IGMConfigT "igm_config")
