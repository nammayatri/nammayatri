{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Storage.TransporterConfig where

import Beckn.Types.Id
import Beckn.Utils.TH
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.Storage.Organization (Organization)

data TransporterConfigT f = TransporterConfig
  { id :: B.C f (Id TransporterParameter),
    transporterId :: B.C f (Id Organization),
    key :: B.C f ConfigKey,
    value :: B.C f Text,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type TransporterConfig = TransporterConfigT Identity

type TransporterConfigPrimaryKey = B.PrimaryKey TransporterConfigT Identity

{-# ANN module ("HLint: ignore Redundant id" :: String) #-}

instance B.Table TransporterConfigT where
  data PrimaryKey TransporterConfigT f = TransporterConfigPrimaryKey (B.C f (Id TransporterParameter))
    deriving (Generic, B.Beamable)
  primaryKey = TransporterConfigPrimaryKey . id

deriving instance FromJSON TransporterConfig

deriving instance ToJSON TransporterConfig

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity TransporterConfigT)
fieldEMod =
  B.setEntityName "transporter_config"
    <> B.modifyTableFields
      B.tableModification
        { transporterId = "transporter_id",
          createdAt = "created_at",
          updatedAt = "updated_at"
        }

data TransporterParameter

newtype ConfigKey = ConfigKey
  { getConfigKey :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''ConfigKey
