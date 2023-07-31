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

module Storage.Beam.Payment.PaymentOrder where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.External.Encryption (DbHash)
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Lib.UtilsTH
import Sequelize

data PaymentOrderT f = PaymentOrderT
  { id :: B.C f Text,
    shortId :: B.C f Text,
    customerId :: B.C f Text,
    merchantId :: B.C f Text,
    amount :: B.C f Money,
    currency :: B.C f Payment.Currency,
    status :: B.C f Payment.TransactionStatus,
    webPaymentLink :: B.C f (Maybe Text),
    iframePaymentLink :: B.C f (Maybe Text),
    mobilePaymentLink :: B.C f (Maybe Text),
    clientAuthTokenEncrypted :: B.C f Text,
    clientAuthTokenHash :: B.C f DbHash,
    clientAuthTokenExpiry :: B.C f Time.UTCTime,
    getUpiDeepLinksOption :: B.C f (Maybe Bool),
    environment :: B.C f (Maybe Text),
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PaymentOrderT where
  data PrimaryKey PaymentOrderT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type PaymentOrder = PaymentOrderT Identity

paymentOrderTMod :: PaymentOrderT (B.FieldModification (B.TableField PaymentOrderT))
paymentOrderTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      shortId = B.fieldNamed "short_id",
      customerId = B.fieldNamed "customer_id",
      merchantId = B.fieldNamed "merchant_id",
      amount = B.fieldNamed "amount",
      currency = B.fieldNamed "currency",
      status = B.fieldNamed "status",
      webPaymentLink = B.fieldNamed "web_payment_link",
      iframePaymentLink = B.fieldNamed "iframe_payment_link",
      mobilePaymentLink = B.fieldNamed "mobile_payment_link",
      clientAuthTokenEncrypted = B.fieldNamed "client_auth_token_encrypted",
      clientAuthTokenHash = B.fieldNamed "client_auth_token_hash",
      clientAuthTokenExpiry = B.fieldNamed "client_auth_token_expiry",
      getUpiDeepLinksOption = B.fieldNamed "get_upi_deep_links_option",
      environment = B.fieldNamed "environment",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''PaymentOrderT ['id] [])

$(mkTableInstances ''PaymentOrderT "payment_order" "atlas_app")
