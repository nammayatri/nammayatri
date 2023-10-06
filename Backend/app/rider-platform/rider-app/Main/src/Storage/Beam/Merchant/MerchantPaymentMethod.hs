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

module Storage.Beam.Merchant.MerchantPaymentMethod where

import qualified Database.Beam as B
import qualified Domain.Types.Merchant.MerchantPaymentMethod as Domain
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH as TH

data MerchantPaymentMethodT f = MerchantPaymentMethodT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    paymentType :: B.C f Domain.PaymentType,
    paymentInstrument :: B.C f Domain.PaymentInstrument,
    collectedBy :: B.C f Domain.PaymentCollector,
    priority :: B.C f Int,
    updatedAt :: B.C f UTCTime,
    createdAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantPaymentMethodT where
  data PrimaryKey MerchantPaymentMethodT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type MerchantPaymentMethod = MerchantPaymentMethodT Identity

$(TH.enableKVPG ''MerchantPaymentMethodT ['id] [['merchantId]])

$(TH.mkTableInstances ''MerchantPaymentMethodT "merchant_payment_method")
