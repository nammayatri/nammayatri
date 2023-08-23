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

module Storage.Beam.Merchant.MerchantPaymentMethod where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.MySQL ()
import Database.Beam.Postgres
  ( Postgres,
  )
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import qualified Domain.Types.Merchant.MerchantPaymentMethod as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import EulerHS.PIIEncryption
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize

instance FromField Domain.PaymentCollector where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.PaymentCollector where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.PaymentCollector

instance FromBackendRow Postgres Domain.PaymentCollector

instance IsString Domain.PaymentCollector where
  fromString = show

instance FromField Domain.PaymentInstrument where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.PaymentInstrument where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.PaymentInstrument

instance FromBackendRow Postgres Domain.PaymentInstrument

instance IsString Domain.PaymentInstrument where
  fromString = show

instance FromField Domain.PaymentType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Domain.PaymentType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be Domain.PaymentType

instance FromBackendRow Postgres Domain.PaymentType

instance IsString Domain.PaymentType where
  fromString = show

data MerchantPaymentMethodT f = MerchantPaymentMethodT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    paymentType :: B.C f Domain.PaymentType,
    paymentInstrument :: B.C f Domain.PaymentInstrument,
    collectedBy :: B.C f Domain.PaymentCollector,
    priority :: B.C f Int,
    updatedAt :: B.C f Time.UTCTime,
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantPaymentMethodT where
  data PrimaryKey MerchantPaymentMethodT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type MerchantPaymentMethod = MerchantPaymentMethodT Identity

merchantPaymentMethodTMod :: MerchantPaymentMethodT (B.FieldModification (B.TableField MerchantPaymentMethodT))
merchantPaymentMethodTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      merchantId = B.fieldNamed "merchant_id",
      paymentType = B.fieldNamed "payment_type",
      paymentInstrument = B.fieldNamed "payment_instrument",
      collectedBy = B.fieldNamed "collected_by",
      priority = B.fieldNamed "priority",
      updatedAt = B.fieldNamed "updated_at",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''MerchantPaymentMethodT ['id] [['merchantId]])

$(mkTableInstances ''MerchantPaymentMethodT "merchant_payment_method" "atlas_app")
