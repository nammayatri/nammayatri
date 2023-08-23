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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.CallbackRequest where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.CallbackRequest as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.PIIEncryption
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption (DbHash)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance FromField Domain.CallbackRequestStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.CallbackRequestStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.CallbackRequestStatus

instance FromBackendRow Postgres Domain.CallbackRequestStatus

instance IsString Domain.CallbackRequestStatus where
  fromString = show

data CallbackRequestT f = CallbackRequestT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    customerName :: B.C f (Maybe Text),
    customerPhoneEncrypted :: B.C f Text,
    customerPhoneHash :: B.C f DbHash,
    customerMobileCountryCode :: B.C f Text,
    status :: B.C f Domain.CallbackRequestStatus,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table CallbackRequestT where
  data PrimaryKey CallbackRequestT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type CallbackRequest = CallbackRequestT Identity

callbackRequestTMod :: CallbackRequestT (B.FieldModification (B.TableField CallbackRequestT))
callbackRequestTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      customerName = B.fieldNamed "customer_name",
      customerPhoneEncrypted = B.fieldNamed "customer_phone_encrypted",
      customerPhoneHash = B.fieldNamed "customer_phone_hash",
      customerMobileCountryCode = B.fieldNamed "customer_mobile_country_code",
      status = B.fieldNamed "status",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''CallbackRequestT ['id] [])

$(mkTableInstances ''CallbackRequestT "callback_request" "atlas_app")
