{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.PassVerifyTransaction where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PurchasedPass
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data PassVerifyTransaction = PassVerifyTransaction
  { destinationStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fleetId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PassVerifyTransaction.PassVerifyTransaction,
    isActuallyValid :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    purchasePassId :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass,
    sourceStopCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    validTill :: Kernel.Prelude.UTCTime,
    verifiedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
