{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.MerchantPaymentMethod (module Domain.Types.MerchantPaymentMethod, module ReExport) where
import Kernel.Prelude
import Domain.Types.Common (UsageSafety (..))
import Data.Aeson
import Domain.Types.Extra.MerchantPaymentMethod as ReExport
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Kernel.Types.Id
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data MerchantPaymentMethodD (s :: UsageSafety)
    = MerchantPaymentMethod {collectedBy :: Domain.Types.Extra.MerchantPaymentMethod.PaymentCollector,
                             createdAt :: Kernel.Prelude.UTCTime,
                             id :: Kernel.Types.Id.Id Domain.Types.MerchantPaymentMethod.MerchantPaymentMethod,
                             merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                             merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
                             paymentInstrument :: Domain.Types.Extra.MerchantPaymentMethod.PaymentInstrument,
                             paymentType :: Domain.Types.Extra.MerchantPaymentMethod.PaymentType,
                             priority :: Kernel.Prelude.Int,
                             updatedAt :: Kernel.Prelude.UTCTime}
    deriving Generic
type MerchantPaymentMethod = MerchantPaymentMethodD ('Safe)
instance FromJSON (MerchantPaymentMethodD 'Unsafe)
instance ToJSON (MerchantPaymentMethodD 'Unsafe)
instance FromJSON (MerchantPaymentMethodD 'Safe)
instance ToJSON (MerchantPaymentMethodD 'Safe)



