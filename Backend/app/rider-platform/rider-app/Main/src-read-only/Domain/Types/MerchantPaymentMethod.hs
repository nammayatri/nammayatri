{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.MerchantPaymentMethod (module Domain.Types.MerchantPaymentMethod, module ReExport) where

import Data.Aeson
import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Extra.MerchantPaymentMethod as ReExport
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data MerchantPaymentMethodD (s :: UsageSafety) = MerchantPaymentMethod
  { id :: Kernel.Types.Id.Id Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    paymentType :: Domain.Types.Extra.MerchantPaymentMethod.PaymentType,
    paymentInstrument :: Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument,
    collectedBy :: Domain.Types.Extra.MerchantPaymentMethod.PaymentCollector,
    priority :: Kernel.Prelude.Int,
    updatedAt :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

type MerchantPaymentMethod = MerchantPaymentMethodD 'Safe

instance FromJSON (MerchantPaymentMethodD 'Unsafe)

instance ToJSON (MerchantPaymentMethodD 'Unsafe)

instance FromJSON (MerchantPaymentMethodD 'Safe)

instance ToJSON (MerchantPaymentMethodD 'Safe)
