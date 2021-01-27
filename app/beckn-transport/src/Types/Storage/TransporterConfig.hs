module Types.Storage.TransporterConfig where

import Beckn.Types.App (OrganizationId)
import Data.Time (UTCTime)
import qualified Database.Beam as B
import EulerHS.Prelude
import Types.App (ConfigKey, TranposrterParameterId (..))

data TransporterConfigT f = TransporterConfig
  { _id :: B.C f TranposrterParameterId,
    _transporterId :: B.C f OrganizationId,
    _key :: B.C f ConfigKey,
    _value :: B.C f Text,
    _createdAt :: B.C f UTCTime,
    _updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

type TransporterConfig = TransporterConfigT Identity

type TransporterConfigPrimaryKey = B.PrimaryKey TransporterConfigT Identity

instance B.Table TransporterConfigT where
  data PrimaryKey TransporterConfigT f = TransporterConfigPrimaryKey (B.C f TranposrterParameterId)
    deriving (Generic, B.Beamable)
  primaryKey = TransporterConfigPrimaryKey . _id

instance ToJSON TransporterConfig where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance FromJSON TransporterConfig where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

fieldEMod ::
  B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity TransporterConfigT)
fieldEMod =
  B.setEntityName "transporter_config"
    <> B.modifyTableFields
      B.tableModification
        { _transporterId = "transporter_id",
          _createdAt = "created_at",
          _updatedAt = "updated_at"
        }
