{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Mandate where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Mandate as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.Prelude (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common (HighPrecMoney, fromFieldEnum)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

data MandateT f = MandateT
  { id :: B.C f Text,
    status :: B.C f Domain.MandateStatus,
    startDate :: B.C f UTCTime,
    endDate :: B.C f UTCTime,
    maxAmount :: B.C f HighPrecMoney,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MandateT where
  data PrimaryKey MandateT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta MandateT where
  modelFieldModification = mandateTMod
  modelTableName = "mandate"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type Mandate = MandateT Identity

instance FromJSON Mandate where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON Mandate where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show Mandate

instance FromBackendRow Postgres Domain.MandateStatus

instance FromField Domain.MandateStatus where
  fromField = fromFieldEnum

mandateTMod :: MandateT (B.FieldModification (B.TableField MandateT))
mandateTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      status = B.fieldNamed "status",
      startDate = B.fieldNamed "startDate",
      endDate = B.fieldNamed "endDate",
      maxAmount = B.fieldNamed "maxAmount",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

instance Serialize Mandate where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

mandateToHSModifiers :: M.Map Text (A.Value -> A.Value)
mandateToHSModifiers =
  M.empty

mandateToPSModifiers :: M.Map Text (A.Value -> A.Value)
mandateToPSModifiers =
  M.empty

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.MandateStatus where
  sqlValueSyntax = autoSqlValueSyntax

deriving stock instance Ord Domain.MandateStatus

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.MandateStatus

$(enableKVPG ''MandateT ['id] [])
