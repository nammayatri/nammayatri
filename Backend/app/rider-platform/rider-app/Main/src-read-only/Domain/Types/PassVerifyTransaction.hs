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
  { fleetId :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.PassVerifyTransaction.PassVerifyTransaction,
    purchasePassId :: Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass,
    validTill :: Kernel.Prelude.UTCTime,
    verifiedAt :: Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
