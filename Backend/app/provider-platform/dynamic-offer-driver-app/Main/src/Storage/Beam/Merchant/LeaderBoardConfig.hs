{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Merchant.LeaderBoardConfig where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Merchant.LeaderBoardConfig as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

instance FromField Domain.LeaderBoardType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.LeaderBoardType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.LeaderBoardType

instance FromBackendRow Postgres Domain.LeaderBoardType

instance IsString Domain.LeaderBoardType where
  fromString = show

data LeaderBoardConfigsT f = LeaderBoardConfigsT
  { id :: B.C f Text,
    leaderBoardType :: B.C f Domain.LeaderBoardType,
    numberOfSets :: B.C f Int,
    leaderBoardExpiry :: B.C f Seconds,
    zScoreBase :: B.C f Int,
    leaderBoardLengthLimit :: B.C f Int,
    isEnabled :: B.C f Bool,
    merchantId :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table LeaderBoardConfigsT where
  data PrimaryKey LeaderBoardConfigsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta LeaderBoardConfigsT where
  modelFieldModification = leaderBoardConfigsTMod
  modelTableName = "leader_board_configs"
  modelSchemaName = Just "atlas_driver_offer_bpp"

type LeaderBoardConfigs = LeaderBoardConfigsT Identity

instance FromJSON LeaderBoardConfigs where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON LeaderBoardConfigs where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show LeaderBoardConfigs

leaderBoardConfigsTMod :: LeaderBoardConfigsT (B.FieldModification (B.TableField LeaderBoardConfigsT))
leaderBoardConfigsTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      leaderBoardType = B.fieldNamed "leader_board_type",
      numberOfSets = B.fieldNamed "number_of_sets",
      leaderBoardExpiry = B.fieldNamed "leader_board_expiry",
      zScoreBase = B.fieldNamed "z_score_base",
      leaderBoardLengthLimit = B.fieldNamed "leader_board_length_limit",
      isEnabled = B.fieldNamed "is_enabled",
      merchantId = B.fieldNamed "merchant_id"
    }

instance Serialize LeaderBoardConfigs where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

leaderBoardConfigsToHSModifiers :: M.Map Text (A.Value -> A.Value)
leaderBoardConfigsToHSModifiers =
  M.empty

leaderBoardConfigsToPSModifiers :: M.Map Text (A.Value -> A.Value)
leaderBoardConfigsToPSModifiers =
  M.empty

$(enableKVPG ''LeaderBoardConfigsT ['id] [['merchantId, 'leaderBoardType]])
