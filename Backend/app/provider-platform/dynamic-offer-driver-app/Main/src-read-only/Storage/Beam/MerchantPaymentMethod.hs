{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.MerchantPaymentMethod where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.MerchantPaymentMethod
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data MerchantPaymentMethodT f = MerchantPaymentMethodT
  { collectedBy :: B.C f Domain.Types.Extra.MerchantPaymentMethod.PaymentCollector,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    paymentInstrument :: B.C f Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument,
    paymentType :: B.C f Domain.Types.Extra.MerchantPaymentMethod.PaymentType,
    priority :: B.C f Kernel.Prelude.Int,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MerchantPaymentMethodT where
  data PrimaryKey MerchantPaymentMethodT f = MerchantPaymentMethodId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = MerchantPaymentMethodId . id

type MerchantPaymentMethod = MerchantPaymentMethodT Identity

$(enableKVPG ''MerchantPaymentMethodT ['id] [])

$(mkTableInstances ''MerchantPaymentMethodT "merchant_payment_method")
