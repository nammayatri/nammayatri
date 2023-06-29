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

module Storage.Beam.DriverOnboarding.AadhaarOtpReq where

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

data AadhaarOtpReqT f = AadhaarOtpReqT
  { id :: B.C f Text,
    driverId :: B.C f Text,
    requestId :: B.C f Text,
    statusCode :: B.C f Text,
    transactionId :: B.C f Text,
    requestMessage :: B.C f Text,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table AadhaarOtpReqT where
  data PrimaryKey AadhaarOtpReqT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

instance ModelMeta AadhaarOtpReqT where
  modelFieldModification = aadhaarOtpReqTMod
  modelTableName = "aadhaar_otp_req"
  modelSchemaName = Just "atlas_app"

type AadhaarOtpReq = AadhaarOtpReqT Identity

instance FromJSON AadhaarOtpReq where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON AadhaarOtpReq where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show AadhaarOtpReq

aadhaarOtpReqTMod :: AadhaarOtpReqT (B.FieldModification (B.TableField AadhaarOtpReqT))
aadhaarOtpReqTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      driverId = B.fieldNamed "driver_id",
      requestId = B.fieldNamed "request_id",
      statusCode = B.fieldNamed "status_code",
      transactionId = B.fieldNamed "transaction_id",
      requestMessage = B.fieldNamed "request_message",
      createdAt = B.fieldNamed "created_at"
    }

defaultAadhaarOtpReq :: AadhaarOtpReq
defaultAadhaarOtpReq =
  AadhaarOtpReqT
    { id = "",
      driverId = "",
      requestId = "",
      statusCode = "",
      transactionId = "",
      requestMessage = "",
      createdAt = defaultUTCDate
    }

instance Serialize AadhaarOtpReq where
  put = error "undefined"
  get = error "undefined"

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

aadhaarOtpReqToHSModifiers :: M.Map Text (A.Value -> A.Value)
aadhaarOtpReqToHSModifiers =
  M.empty

aadhaarOtpReqToPSModifiers :: M.Map Text (A.Value -> A.Value)
aadhaarOtpReqToPSModifiers =
  M.empty

$(enableKVPG ''AadhaarOtpReqT ['id] [])
