{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Lib.Payment.Storage.Beam.PaymentOrder where

import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.External.Encryption (DbHash)
import qualified Kernel.External.Payment.Interface as Payment
import Kernel.Prelude
import Kernel.Types.Common hiding (Price (..), PriceAPIEntity (..), id)
import Lib.Payment.Domain.Types.Common (EntityName, PaymentFulfillmentStatus)
import qualified Lib.Payment.Domain.Types.PaymentOrder

data PaymentOrderT f = PaymentOrderT
  { id :: B.C f Text,
    shortId :: B.C f Text,
    paymentServiceOrderId :: B.C f Text,
    personId :: B.C f Text,
    merchantId :: B.C f Text,
    entityName :: B.C f (Maybe EntityName),
    paymentServiceType :: B.C f (Maybe Lib.Payment.Domain.Types.PaymentOrder.PaymentServiceType),
    paymentMerchantId :: B.C f (Maybe Text),
    requestId :: B.C f (Maybe Text),
    service :: B.C f (Maybe Text),
    clientId :: B.C f (Maybe Text),
    description :: B.C f (Maybe Text),
    returnUrl :: B.C f (Maybe Text),
    action :: B.C f (Maybe Text),
    amount :: B.C f HighPrecMoney, -- FIXME Kernel.Types.Common.Price
    currency :: B.C f Currency, -- FIXME Kernel.Types.Common.Price
    status :: B.C f Payment.TransactionStatus,
    webPaymentLink :: B.C f (Maybe Text),
    iframePaymentLink :: B.C f (Maybe Text),
    mobilePaymentLink :: B.C f (Maybe Text),
    deepLink :: B.C f (Maybe Text),
    clientAuthTokenEncrypted :: B.C f (Maybe Text),
    clientAuthTokenHash :: B.C f (Maybe DbHash),
    clientAuthTokenExpiry :: B.C f (Maybe UTCTime),
    getUpiDeepLinksOption :: B.C f (Maybe Bool),
    environment :: B.C f (Maybe Text),
    createMandate :: B.C f (Maybe Payment.MandateType),
    mandateMaxAmount :: B.C f (Maybe HighPrecMoney), -- FIXME Kernel.Types.Common.Price
    isRetried :: B.C f Bool,
    isRetargeted :: B.C f Bool,
    retargetLink :: B.C f (Maybe Text),
    mandateStartDate :: B.C f (Maybe UTCTime),
    mandateEndDate :: B.C f (Maybe UTCTime),
    bankErrorCode :: B.C f (Maybe Text),
    bankErrorMessage :: B.C f (Maybe Text),
    serviceProvider :: B.C f (Maybe Payment.PaymentService),
    sdkPayloadDump :: B.C f (Maybe Value),
    validTill :: B.C f (Maybe UTCTime),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    merchantOperatingCityId :: B.C f (Maybe Text),
    effectiveAmount :: B.C f (Maybe HighPrecMoney),
    paymentFulfillmentStatus :: B.C f (Maybe PaymentFulfillmentStatus)
  }
  deriving (Generic, B.Beamable)

instance B.Table PaymentOrderT where
  data PrimaryKey PaymentOrderT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type PaymentOrder = PaymentOrderT Identity

$(enableKVPG ''PaymentOrderT ['id] [['shortId], ['personId]])

$(mkTableInstancesGenericSchema ''PaymentOrderT "payment_order")
