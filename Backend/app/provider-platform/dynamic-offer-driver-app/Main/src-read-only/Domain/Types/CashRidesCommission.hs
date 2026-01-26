{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.CashRidesCommission where

import Data.Aeson
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Kernel.External.Payment.Stripe.Types.Transfer
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data CashRidesCommission = CashRidesCommission
  { amount :: Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    id :: Kernel.Types.Id.Id Domain.Types.CashRidesCommission.CashRidesCommission,
    lastSettlementTime :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    nextSettlementTime :: Kernel.Prelude.UTCTime,
    numberOfRides :: Kernel.Prelude.Int,
    paymentMode :: Domain.Types.Extra.MerchantPaymentMethod.PaymentMode,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    personRole :: Domain.Types.Person.Role,
    status :: Kernel.External.Payment.Stripe.Types.Transfer.TransferStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
