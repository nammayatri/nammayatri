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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.Merchant.MerchantMessage where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Merchant.MerchantMessage as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance FromField Domain.MessageKey where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.MessageKey where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.MessageKey

instance FromBackendRow Postgres Domain.MessageKey

instance IsString Domain.MessageKey where
  fromString = show

data MerchantMessageT f = MerchantMessageT
  { merchantId :: B.C f Text,
    messageKey :: B.C f Domain.MessageKey,
    message :: B.C f Text,
    updatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantMessageT where
  data PrimaryKey MerchantMessageT f
    = Id (B.C f Domain.MessageKey)
    deriving (Generic, B.Beamable)
  primaryKey = Id . messageKey

type MerchantMessage = MerchantMessageT Identity

merchantMessageTMod :: MerchantMessageT (B.FieldModification (B.TableField MerchantMessageT))
merchantMessageTMod =
  B.tableModification
    { merchantId = B.fieldNamed "merchant_id",
      messageKey = B.fieldNamed "message_key",
      message = B.fieldNamed "message",
      updatedAt = B.fieldNamed "updated_at",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''MerchantMessageT ['merchantId, 'messageKey] [])

$(mkTableInstances ''MerchantMessageT "merchant_message" "atlas_driver_offer_bpp")
