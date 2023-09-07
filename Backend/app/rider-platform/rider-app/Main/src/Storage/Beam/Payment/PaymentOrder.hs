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

module Storage.Beam.Payment.PaymentOrder where

import qualified Database.Beam as B
import Tools.Beam.UtilsTH
import Kernel.External.Encryption (DbHash)
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Common hiding (id)

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
    clientAuthTokenExpiry :: B.C f UTCTime,
    getUpiDeepLinksOption :: B.C f (Maybe Bool),
    environment :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PaymentOrderT where
  data PrimaryKey PaymentOrderT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type PaymentOrder = PaymentOrderT Identity

$(enableKVPG ''PaymentOrderT ['id] [])

$(mkTableInstances ''PaymentOrderT "payment_order")
