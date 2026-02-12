{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverBankAccount where

import Data.Aeson
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.External.Payment.Stripe.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Servant.Client.Core
import qualified Tools.Beam.UtilsTH

data DriverBankAccount = DriverBankAccount
  { accountId :: Kernel.External.Payment.Stripe.Types.AccountId,
    chargesEnabled :: Kernel.Prelude.Bool,
    currentAccountLink :: Kernel.Prelude.Maybe Servant.Client.Core.BaseUrl,
    currentAccountLinkExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    detailsSubmitted :: Kernel.Prelude.Bool,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    ifscCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    nameAtBank :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    paymentMode :: Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
