{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantPaymentMethod (module Domain.Types.MerchantPaymentMethod, module ReExport) where

import qualified BecknV2.OnDemand.FareBreakup.Tax
import Data.Aeson
import Domain.Types.Extra.MerchantPaymentMethod as ReExport
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PaymentChargeBearer
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantPaymentMethod = MerchantPaymentMethod
  { collectedBy :: Domain.Types.Extra.MerchantPaymentMethod.PaymentCollector,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    paymentCharge :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    paymentChargeBearer :: Kernel.Prelude.Maybe Domain.Types.PaymentChargeBearer.PaymentChargeBearer,
    paymentChargeTax :: Kernel.Prelude.Maybe BecknV2.OnDemand.FareBreakup.Tax.TaxConfig,
    paymentChargeType :: Kernel.Prelude.Maybe Domain.Types.PaymentChargeBearer.PaymentChargeType,
    paymentInstrument :: Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument,
    paymentType :: Domain.Types.Extra.MerchantPaymentMethod.PaymentType,
    priority :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
