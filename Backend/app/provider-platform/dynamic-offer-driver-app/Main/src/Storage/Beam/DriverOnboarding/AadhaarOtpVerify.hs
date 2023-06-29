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

module Storage.Beam.DriverOnboarding.AadhaarOtpVerify where

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

data AadhaarOtpVerifyT f = AadhaarOtpVerifyT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    requestId :: B.C f Text,
    statusCode :: B.C f Text,
    transactionId :: B.C f Text,
    requestMessage :: B.C f Text,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AadhaarOtpVerifyT where
  data PrimaryKey AadhaarOtpVerifyT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta AadhaarOtpVerifyT where
  modelFieldModification = aadhaarOtpVerifyTMod
  modelTableName = "aadhaar_otp_verify"
  modelSchemaName = Just "atlas_app"

type AadhaarOtpVerify = AadhaarOtpVerifyT Identity

instance FromJSON AadhaarOtpVerify where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON AadhaarOtpVerify where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show AadhaarOtpVerify

aadhaarOtpVerifyTMod :: AadhaarOtpVerifyT (B.FieldModification (B.TableField AadhaarOtpVerifyT))
aadhaarOtpVerifyTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driver_id",
      requestId = B.fieldNamed "request_id",
      statusCode = B.fieldNamed "status_code",
      transactionId = B.fieldNamed "transaction_id",
      requestMessage = B.fieldNamed "request_message",
      createdAt = B.fieldNamed "created_at"
    }

defaultAadhaarOtpVerify :: AadhaarOtpVerify
defaultAadhaarOtpVerify =
  AadhaarOtpVerifyT
    { id = "",
      driverId = "",
      requestId = "",
      statusCode = "",
      transactionId = "",
      requestMessage = "",
      createdAt = defaultUTCDate
    }

instance Serialize AadhaarOtpVerify where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

aadhaarOtpVerifyToHSModifiers :: M.Map Text (A.Value -> A.Value)
aadhaarOtpVerifyToHSModifiers =
  M.empty

aadhaarOtpVerifyToPSModifiers :: M.Map Text (A.Value -> A.Value)
aadhaarOtpVerifyToPSModifiers =
  M.empty

$(enableKVPG ''AadhaarOtpVerifyT ['id] [])
