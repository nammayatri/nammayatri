{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.DriverReferral where

import qualified Data.Aeson as A
import Data.ByteString.Internal (ByteString, unpackChars)
import qualified Data.HashMap.Internal as HM
import qualified Data.Map.Strict as M
import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
    ResultError (ConversionFailed, UnexpectedNull),
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.DriverReferral as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils
import Lib.UtilsTH
import Sequelize
import Storage.Tabular.Person (PersonTId)

-- fromFieldEnum ::
--   (Typeable a, Read a) =>
--   DPSF.Field ->
--   Maybe ByteString ->
--   DPSF.Conversion a
-- fromFieldEnum f mbValue = case mbValue of
--   Nothing -> DPSF.returnError UnexpectedNull f mempty
--   Just value' ->
--     case (readMaybe (unpackChars value')) of
--       Just val -> pure val
--       _ -> DPSF.returnError ConversionFailed f "Could not 'read' value for 'Rule'."

data DriverReferralT f = DriverReferralT
  { referralCode :: B.C f Text,
    driverId :: B.C f Text,
    linkedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverReferralT where
  data PrimaryKey DriverReferralT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . referralCode

instance ModelMeta DriverReferralT where
  modelFieldModification = driverReferralTMod
  modelTableName = "driver_referral"
  mkExprWithDefault _ = B.insertExpressions []

type DriverReferral = DriverReferralT Identity

instance FromJSON DriverReferral where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON DriverReferral where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Show DriverReferral

driverReferralTMod :: DriverReferralT (B.FieldModification (B.TableField DriverReferralT))
driverReferralTMod =
  B.tableModification
    { referralCode = B.fieldNamed "referral_code",
      driverId = B.fieldNamed "driver_id",
      linkedAt = B.fieldNamed "linked_at"
    }

psToHs :: HM.HashMap Text Text
psToHs = HM.empty

driverReferralToHSModifiers :: M.Map Text (A.Value -> A.Value)
driverReferralToHSModifiers =
  M.fromList
    []

driverReferralToPSModifiers :: M.Map Text (A.Value -> A.Value)
driverReferralToPSModifiers =
  M.fromList
    []

defaultDriverReferral :: DriverReferral
defaultDriverReferral =
  DriverReferralT
    { referralCode = "",
      driverId = "",
      linkedAt = defaultUTCDate
    }

instance Serialize DriverReferral where
  put = error "undefined"
  get = error "undefined"

$(enableKVPG ''DriverReferralT ['referralCode] [])
