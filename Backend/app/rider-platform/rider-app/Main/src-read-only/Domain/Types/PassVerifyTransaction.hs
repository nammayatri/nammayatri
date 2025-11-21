{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PassVerifyTransaction where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PurchasedPass
import qualified Domain.Types.PurchasedPassPayment
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PassVerifyTransaction = PassVerifyTransaction
  { closingAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    destinationStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    entryGateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    exitGateId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PassVerifyTransaction.PassVerifyTransaction,
    openingAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    purchasePassId :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass,
    purchasePassPaymentId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PurchasedPassPayment.PurchasedPassPayment),
    sourceStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    validTill :: Kernel.Prelude.UTCTime,
    verifiedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
