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

module Storage.Beam.AppInstalls where

import qualified Data.Aeson as A
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Lib.Utils
import Lib.UtilsTH
import Sequelize

data AppInstallsT f = AppInstallsT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    deviceToken :: B.C f Text,
    source :: B.C f Text,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime,
    appVersion :: B.C f (Maybe Text),
    bundleVersion :: B.C f (Maybe Text),
    platform :: B.C f (Maybe Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table AppInstallsT where
  data PrimaryKey AppInstallsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta AppInstallsT where
  modelFieldModification = appInstallsTMod
  modelTableName = "app_installs"
  modelSchemaName = Just "atlas_app"

type AppInstalls = AppInstallsT Identity

instance FromJSON AppInstalls where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON AppInstalls where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show AppInstalls

appInstallsTMod :: AppInstallsT (B.FieldModification (B.TableField AppInstallsT))
appInstallsTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      deviceToken = B.fieldNamed "device_token",
      source = B.fieldNamed "source",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at",
      appVersion = B.fieldNamed "app_version",
      bundleVersion = B.fieldNamed "bundle_version",
      platform = B.fieldNamed "platform"
    }

defaultAppInstalls :: AppInstalls
defaultAppInstalls =
  AppInstallsT
    { id = "",
      merchantId = "",
      deviceToken = "",
      source = "",
      createdAt = defaultUTCDate,
      updatedAt = defaultUTCDate,
      appVersion = Nothing,
      bundleVersion = Nothing,
      platform = Nothing
    }

instance Serialize AppInstalls where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

appInstallsToHSModifiers :: M.Map Text (A.Value -> A.Value)
appInstallsToHSModifiers =
  M.empty

appInstallsToPSModifiers :: M.Map Text (A.Value -> A.Value)
appInstallsToPSModifiers =
  M.empty

$(enableKVPG ''AppInstallsT ['id] [])
