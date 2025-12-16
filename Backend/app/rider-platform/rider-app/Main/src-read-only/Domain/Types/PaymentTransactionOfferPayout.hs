{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PaymentTransactionOfferPayout where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PaymentTransactionOfferPayout = PaymentTransactionOfferPayout
  { amountPaidByUser :: Kernel.Types.Common.HighPrecMoney,
    bookingCreatedAt :: Kernel.Prelude.UTCTime,
    createdAt :: Kernel.Prelude.UTCTime,
    id :: Kernel.Types.Id.Id Domain.Types.PaymentTransactionOfferPayout.PaymentTransactionOfferPayout,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    networkOrderId :: Kernel.Prelude.Text,
    offerCode :: Kernel.Prelude.Text,
    offerId :: Kernel.Prelude.Text,
    orderAmount :: Kernel.Types.Common.HighPrecMoney,
    paymentRRN :: Kernel.Prelude.Text,
    paymentTxnId :: Kernel.Prelude.Text,
    providerAgency :: Kernel.Prelude.Text,
    settlementReference :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    settlementTs :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
