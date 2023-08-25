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

module Lib.Payment.Storage.Beam.PaymentOrderDriver where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption (DbHash)
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import qualified Lib.Payment.Domain.Types.PaymentOrder ()
import Sequelize

data PaymentOrderT f = PaymentOrderT
  { id :: B.C f Text,
    shortId :: B.C f Text,
    paymentServiceOrderId :: B.C f Text,
    personId :: B.C f Text,
    merchantId :: B.C f Text,
    paymentMerchantId :: B.C f (Maybe Text),
    requestId :: B.C f (Maybe Text),
    service :: B.C f (Maybe Text),
    clientId :: B.C f (Maybe Text),
    description :: B.C f (Maybe Text),
    returnUrl :: B.C f (Maybe Text),
    action :: B.C f (Maybe Text),
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
    createMandate :: B.C f (Maybe Payment.MandateType),
    mandateMaxAmount :: B.C f (Maybe HighPrecMoney),
    mandateStartDate :: B.C f (Maybe Time.UTCTime),
    mandateEndDate :: B.C f (Maybe Time.UTCTime),
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
      paymentServiceOrderId = B.fieldNamed "payment_service_order_id",
      personId = B.fieldNamed "person_id",
      merchantId = B.fieldNamed "merchant_id",
      paymentMerchantId = B.fieldNamed "payment_merchant_id",
      requestId = B.fieldNamed "request_id",
      service = B.fieldNamed "service",
      clientId = B.fieldNamed "client_id",
      description = B.fieldNamed "description",
      returnUrl = B.fieldNamed "return_url",
      action = B.fieldNamed "action",
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
      createMandate = B.fieldNamed "create_mandate",
      mandateMaxAmount = B.fieldNamed "mandate_max_amount",
      mandateStartDate = B.fieldNamed "mandate_start_date",
      mandateEndDate = B.fieldNamed "mandate_end_date",
      createdAt = B.fieldNamed "created_at",
      updatedAt = B.fieldNamed "updated_at"
    }

$(enableKVPG ''PaymentOrderT ['id] [['shortId]])

$(mkTableInstances ''PaymentOrderT "payment_order" "atlas_driver_offer_bpp")
